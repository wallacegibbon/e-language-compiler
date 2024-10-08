%% vim:ft=elang:ts=8:sw=8:sts=8:noet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Application related struct definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
struct LightInterface
	on			: fn(LightInterface^^);
	off			: fn(LightInterface^^);
	toggle			: fn(LightInterface^^; word^): word;
end

struct LED
	light_interface		: LightInterface^;
	port			: GPIO^;
	pin			: word;
	current_state		: word;
	toggle_count		: word;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The global light interface object for LED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LED_light_interface: LightInterface = LightInterface{
	on			= LED_on,
	off			= LED_off,
	toggle			= LED_toggle,
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
fn toggle_light(light: LightInterface^^; r: word^)
	light^^.toggle(light, r);
end

fn on_light(light: LightInterface^^)
	light^^.on(light);
end

fn off_light(light: LightInterface^^)
	light^^.off(light);
end

struct AppState
	lights			: {LightInterface^^, 4};
	leds			: {LED, 4}; %% The inner data
	light_nums		: word;
	delay			: word;
	selected		: word;
end

fn AppState_init(self: AppState^)
	led: LED^;
	i: word = 0;

	self^.light_nums = 4;
	self^.delay = 1;
	self^.selected = 0;

	while i < self^.light_nums do
		led = self^.leds@[i]@;
		LED_init(led, gpio_d, i);
		self^.lights@[i] = led;
		i += 1;
	end
end

fn AppState_all_off(self: AppState^)
	i: word = 0;
	while i < self^.light_nums do
		off_light(self^.lights@[i]);
		i += 1;
	end
end

fn AppState_all_bright(self: AppState^)
	i: word = 0;
	while i < self^.light_nums do
		on_light(self^.lights@[i]);
		i += 1;
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
	tmp: word;
	toggle_light(self^.lights@[0], tmp@);
	toggle_light(self^.lights@[1], tmp@);
	off_light(self^.lights@[2]);
	off_light(self^.lights@[3]);
end

fn AppState_toggle_pair2(self: AppState^)
	tmp: word;
	toggle_light(self^.lights@[2], tmp@);
	toggle_light(self^.lights@[3], tmp@);
	off_light(self^.lights@[0]);
	off_light(self^.lights@[1]);
end

fn AppState_loop_once(self: AppState^)
	if self^.selected == 0 then
		AppState_toggle_pair1(self);
	else
		AppState_toggle_pair2(self);
	end
end

%% Make the app state global for easier debugging.
global_state: AppState = AppState{};

fn main(a1: word)
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
		tmp = 50000;
		while tmp > 0 do
			tmp -= 1;
		end
		count -= 1;
	end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MCU related struct definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
struct GPIO
	CFG_L			: word; %% Port configuration register low
	CFG_H			: word; %% Port configuration register high
	IN			: word; %% Port input data register
	OUT			: word; %% Port output data register
	BSH			: word; %% Port set/reset register
	BC			: word; %% Port reset register
	LOCK			: word; %% Port configuration lock register
end

struct EXTI
	INTEN			: word; %% Interrupt enable register
	EVEN			: word; %% Event enable register
	RTEN			: word; %% Rising edge trigger enable register
	FTEN			: word; %% Falling edge trigger enable register
	SWIEV			: word; %% Software interrupt event register
	INTF			: word; %% Interrupt flag register
end

struct GPTM
	CTL1			: word; %% Control register1
	CTL2			: word; %% Control register2
	SMCFG			: word; %% Slave mode configuration register
	DMAINTEN		: word; %% DMA/interrupt enable register
	INTF			: word; %% Interrupt flag register
	SWEVG			: word; %% Event generation register
	CHCTL1			: word; %% Compare/Capture control register1
	CHCTL2			: word; %% Compare/Capture control register2
	CCEN			: word; %% compare/capture enable register
	CNT			: word; %% Counter
	PSC			: word; %% Prescaler
	ATRL			: word; %% Auto-reload register
	CH1CV			: word; %% Compare/Capture register1
	CH2CV			: word; %% Compare/Capture register2
	CH3CV			: word; %% Compare/Capture register3
	CH4CV			: word; %% Compare/Capture register4
	DMACFG			: word; %% DMA configuration register
	DMAAD			: word; %% DMA address register in continuous mode
	AUX			: word; %% Dual-edge capture register
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Global variables for register banks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gpio_d		: GPIO^ = 0x40011400 as (GPIO^);
exti4		: EXTI^ = 0x40010400 as (EXTI^);
tim2		: GPTM^ = 0x40000000 as (GPTM^);

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
	%% RCC_APB2PCENR, enable clock for PD and AFIO
	(0x40021018 as (word^))^ = 0b00100001;
	%% RCC_APB1PCENR, enable clock for TIM2
	(0x4002101C as (word^))^ = 0b1;

	%% Set PD0~3 as Push-Pull output, PD4 as floating input.
	gpio_d^.CFG_L = 0x44483333;
	gpio_d^.BSH = 0b11111;

	%% EXTI_FTENR, enable falling edge detecting for EXTI4
	exti4^.FTEN = 0b10000;
	exti4^.RTEN = 0b0;
	%% Enable EXTI4.
	exti4^.INTEN = 0b10000;

	%% AFIO_EXTICR2, Connect EXTI4 to PD4.
	(0x4001000C as (word^))^ = 0b0011;

	tim2^.ATRL = 8000;
	tim2^.PSC = 9; % 10ms
	tim2^.PSC = 19; % 20ms
	tim2^.DMAINTEN = 0b1;
	%tim2^.CTL1 = 0b10000001;
	%tim2^.CTL1 = 0b10001001;
	tim2^.CTL1 = 0b0;

	%% PFIC_IENR1, enable the interrupt for EXTI4(id: 26).
	(0xE000E100 as (word^))^ = 1 bsl 26;
	%% PFIC_IENR2, enable the interrupt for TIM2(id: 44). (44 - 32 -> 12)
	(0xE000E104 as (word^))^ = 1 bsl 12;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ISRs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interrupt(26)
fn exit4_isr()
	%% Clear interrupt flag.
	exti4^.INTF = 0b10000;

	%% Restart the timer, Single pulse mode, Auto reload.
	tim2^.CTL1 = 0;
	tim2^.CNT = 0;
	tim2^.CTL1 = 0b10001001;
end

interrupt(44)
fn tim2_isr()
	%% Clear interrupt flag
	tim2^.INTF = 0b0;
	tim2^.CNT = 0;

	if (gpio_d^.IN band 0b10000) != 0 then
		return;
	end

	AppState_switch_selected(global_state@);
	%AppState_adjust_speed(global_state@);
end

