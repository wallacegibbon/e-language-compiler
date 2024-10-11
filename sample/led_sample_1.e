%% vim:ft=elang:ts=8:sw=8:sts=8:noet
%% This program is designed for CH32V3xx. You need to change the code if you run it on CH32V1xx or CH32V2xx.

unused_string1: byte^ = "this string is unused, just for testing linking.";
unused_string2: byte^ = "this string is unused, either.";

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

