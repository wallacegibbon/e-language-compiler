struct LightInterface
	toggle			: fn(LightInterface^^; word^): word;
end

struct LED
	light_interface		: LightInterface^;
	toggle_count		: word;
	port			: any^;
	pin			: word;
	current_state		: word;
end

%% The global light interface object for LED
LED_light_interface: LightInterface = LightInterface{
	toggle			= LED_toggle,
};

fn LED_init(self: LED^; port: any^; pin: word): word
	self^.light_interface = LED_light_interface@;
	self^.port = port;
	self^.pin = pin;
	self^.toggle_count = 0;
	return 0;
end

fn LED_toggle(self: LED^; state: word^): word
	new_state: word = bnot(self^.current_state);
	GPIO_set_state(self^.port, self^.pin, new_state);
	self^.toggle_count += 1;
	state^ = new_state;
	return 0;
end

fn myapp(light: LightInterface^^; r: word^)
	light^^.toggle(light, r);
end

fn main(a1: word)
	led1: LED;
	tmp: word;

	system_init();

	LED_init(led1@, 32 as (any^), 10);
	myapp(led1@, tmp@);
end

fn system_init()
end

fn GPIO_set_state(port: any^; pin: word; state: word)
end

