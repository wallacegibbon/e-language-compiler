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
	self^.current_state = 0;
	self^.toggle_count = 0;
	return 0;
end

fn LED_on(self: LED^)
	GPIO_set_state(self^.port, self^.pin, 0);
end

fn LED_off(self: LED^)
	GPIO_set_state(self^.port, self^.pin, 1);
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

fn my_loop(leds: LED^)
	i: word = 0;
	tmp: word;

	while i < 4 do
		toggle_light(leds + (i * sizeof(LED)), tmp@);
		i += 1;
	end
end

fn all_bright(leds: LED^)
	i: word = 0;
	tmp: word;

	while i < 4 do
		LED_on(leds + (i * sizeof(LED)));
		i += 1;
	end
end

global_delay: word = 5;

fn main(a1: word)
	leds: {LED, 4};
	i: word;

	system_init();

	i = 0;
	while i < 4 do
		LED_init(leds@ + (i * sizeof(LED)), 0x40011400 as (GPIO^), i);
		i += 1;
	end

	toggle_light(leds@ + (0 * sizeof(LED)), i@);
	toggle_light(leds@ + (2 * sizeof(LED)), i@);

	while 1 == 1 do
		my_loop(leds@);
		delay(global_delay);
	end
end

fn delay(count: word)
	tmp: word;
	while count > 0 do
		tmp = 100000;
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
	CFG_L			: word;
	CFG_H			: word;
	IN			: word;
	OUT			: word;
	BSHR			: word;
	BCR			: word;
	LOCK			: word;
end

fn GPIO_set_state(port: GPIO^; pin: word; state: word)
	if state == 0 then
		port^.BCR = 1 bsl pin;
	else
		port^.BSHR = 1 bsl pin;
	end
end

fn system_init()
	%% RCC_APB2PCENR, enable clock for PD and AFIO
	%(0x40021018 as (word^))^ = 1 bsl 5;
	(0x40021018 as (word^))^ = 0x21;

	%% EXTI_INTENR, enable EXTI4.
	(0x40010400 as (word^))^ = 1 bsl 4;
	%% EXTI_FTENR, enable falling edge detecting for EXTI4
	(0x4001040C as (word^))^ = 1 bsl 4;

	%% AFIO_EXTICR2, Connect EXTI4 to PD4.
	(0x4001000C as (word^))^ = 0x0003;

	port: GPIO^ = 0x40011400 as (GPIO^);
	%% Set PD0~3 as Push-Pull output, PD4 as Input (Pull up).
	port^.CFG_L = 0x44481111;
	port^.BSHR = 1 bsl 4;

	%% PFIC_IENR1, enable the interrupt for EXTI4(id: 26).
	(0xE000E100 as (word^))^ = 1 bsl 26;
end

interrupt(26)
fn button_handler()
	%% check whether exti4 is the source
	if ((0x40010414 as (word^))^ band (1 bsl 4)) == 0 then
		goto finish;
	end
	if global_delay == 5 then
		global_delay = 1;
	else
		global_delay = 5;
	end

@@finish
	%% EXTI_INTFR, clear interrupt flag.
	(0x40010414 as (word^))^ = 1 bsl 4;
end

