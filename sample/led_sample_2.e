%% vim:ft=elang:ts=8:sw=8:sts=8:noet
%% This program is designed for CH32V003. To compile it:
%% ec -i ./sample/ch32v.e ./sample/led_sample_2.e -o /tmp/a --v-pos 0 --v-size 156 --c-pos 156 --d-pos 0x2000_0000 --d-size 2K --v-init-jump --prefer-shift

unused_string1: byte^ = "this string is unused, just for testing linking.";
unused_string2: byte^ = "this string is unused, either.";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application related struct definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
struct LightInterface
    on : fn(self: LightInterface^^);
    off : fn(self: LightInterface^^);
    toggle : fn(self: LightInterface^^; state: word^): word;
end

struct LED
    light_interface : LightInterface^;
    port : GPIO^;
    pin : word;
    current_state : word;
    toggle_count : word;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The global light interface object for LED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LED_light_interface: LightInterface = LightInterface{
    on = LED_on,
    off = LED_off,
    toggle = LED_toggle,
};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Methods for LED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fn LED_init(self: LED^; port: GPIO^; pin: word): word
    self^.light_interface = LED_light_interface@;
    self^.port = port;
    self^.pin = pin;
    self^.current_state = bnot(0);
    self^.toggle_count = 0;
    return 0;
end

fn LED_on(self: LED^)
    GPIO_set_state(self^.port, self^.pin, 0);
end

fn LED_off(self: LED^)
    GPIO_set_state(self^.port, self^.pin, bnot(0));
end

fn LED_toggle(self: LED^; state: word^): word
    new_state: word = bnot(self^.current_state);

    GPIO_set_state(self^.port, self^.pin, new_state);
    self^.current_state = new_state;
    self^.toggle_count += 1;

    state^ = new_state;
    return 0;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
struct AppState
    lights : {LightInterface^^, 2};
    leds : {LED, 2}; %% The inner data
    light_nums : word;
    delay : word;
    selected : word;
end

struct AppIter
    app : AppState^;
    cursor : word;
end

fn AppIter_init(self: AppIter^; app: AppState^)
    self^.app = app;
    self^.cursor = 0;
end

fn AppIter_next(self: AppIter^; light: LightInterface^^^; index: word^): word
    if self^.cursor >= self^.app^.light_nums then
        return 1;
    end
    light^ = self^.app^.lights@[self^.cursor];
    index^ = self^.cursor;
    self^.cursor += 1;
    return 0;
end

fn AppState_init(self: AppState^)
    led: LED^;
    i: word = 0;

    self^.light_nums = 2;
    self^.delay = 1;
    self^.selected = 0;

    while i < self^.light_nums do
        self^.lights@[i] = self^.leds@[i]@;
        i += 1;
    end

    iter: AppIter;
    AppIter_init(iter@, self);

    LED_init(self^.lights@[0], ?GPIOD, 0);
    LED_init(self^.lights@[1], ?GPIOD, 2);
end

fn AppState_all_off(self: AppState^)
    iter: AppIter;
    AppIter_init(iter@, self);

    light: LightInterface^^;
    i: word;
    while AppIter_next(iter@, light@, i@) == 0 do
        light^^.off(light);
    end
end

fn AppState_all_bright(self: AppState^)
    iter: AppIter;
    AppIter_init(iter@, self);

    light: LightInterface^^;
    i: word;
    while AppIter_next(iter@, light@, i@) == 0 do
        light^^.on(light);
    end
end

fn AppState_switch_selected(self: AppState^)
    if self^.selected == 0 then
        self^.selected = 1;
    else
        self^.selected = 0;
    end
end

fn AppState_adjust_speed(self: AppState^)
    if self^.delay == 1 then
        self^.delay = 5;
    else
        self^.delay = 1;
    end
end

fn AppState_toggle_pair1(self: AppState^)
    light: LightInterface^^;
    tmp: word;

    light = self^.lights@[0];
    light^^.toggle(light, tmp@);
end

fn AppState_toggle_pair2(self: AppState^)
    light: LightInterface^^;
    tmp: word;

    light = self^.lights@[1];
    light^^.toggle(light, tmp@);
end

fn AppState_loop_once(self: AppState^)
    light: LightInterface^^;
    if self^.selected == 0 then
        AppState_toggle_pair1(self);
        light = self^.lights@[1];
        light^^.off(light);
    else
        AppState_toggle_pair2(self);
        light = self^.lights@[0];
        light^^.off(light);
    end
end

%% Make the app state global for easier debugging.
global_state: AppState = AppState{};

fn main()
    system_init();
    AppState_init(global_state@);

    while 1 == 1 do
        AppState_loop_once(global_state@);
        delay(global_state.delay);
    end
end

fn delay(count: word)
    tmp: word;
    while count > 0 do
        tmp = 80000;
        while tmp > 0 do
            tmp -= 1;
        end
        count -= 1;
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ISRs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fn exit4_isr() attribute(interrupt(20))
    %% Clear interrupt flag.
    ?EXTI^.INTF = 0b1_0000;

    %% Restart the timer, Single pulse mode, Auto reload.
    ?TIM2^.CTL1 = 0;
    ?TIM2^.CNT = 0;
    ?TIM2^.CTL1 = 0b1000_1001;
end

fn TIM2_isr() attribute(interrupt(38))
    %% Clear interrupt flag
    ?TIM2^.INTF = 0b0;
    ?TIM2^.CNT = 0;

    if (?GPIOD^.IN band 0b1_0000) != 0 then
        return;
    end

    AppState_switch_selected(global_state@);
    %AppState_adjust_speed(global_state@);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MCU related functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fn GPIO_set_state(port: GPIO^; pin: word; state: word)
    if state == 0 then
        port^.BC = 1 bsl pin;
    else
        port^.BSH = 1 bsl pin;
    end
end

fn system_init()
    %% Enable clock for PD and AFIO
    ?RCC_APB2CEN^ = 0b0010_0001;
    %% Enable clock for TIM2
    ?RCC_APB1CEN^ = 0b1;

    %% Set PD0~3 as Push-Pull output, PD4 as floating input.
    ?GPIOD^.BSH = 0b1_1101;
    ?GPIOD^.CFG_L = 0x4448_3343;

    %% EXTI_FTENR, enable falling edge detecting for EXTI4
    ?EXTI^.FTEN = 0b1_0000;
    ?EXTI^.RTEN = 0b0;
    %% Enable EXTI4.
    ?EXTI^.INTEN = 0b1_0000;

    %% Connect EXTI4 to PD4.
    ?AFIO_EXTICx[0] = 0b11_0000_0000;

    %% We use the internal 8MHz as system clock.
    ?TIM2^.ATRL = 8000;
    %?TIM2^.PSC = 9; % 10ms
    ?TIM2^.PSC = 19; % 20ms
    ?TIM2^.DMAINTEN = 0b1;
    %?TIM2^.CTL1 = 0b1000_0001;
    %?TIM2^.CTL1 = 0b1000_1001;
    ?TIM2^.CTL1 = 0b0;

    %% Enable the interrupt for EXTI4(id: 20).
    ?PFIC_IENx[0] = 1 bsl 20;
    %% Enable the interrupt for TIM2(id: 38). (38 - 32 -> 6)
    ?PFIC_IENx[1] = 1 bsl 6;
end

