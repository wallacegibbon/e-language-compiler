struct Light
	on: fn(any^): word;
	off: fn(any^): word;
	toggle: fn(any^): word;
end

struct IIC_LED
	interface: Light^;
	port: word;
	address: byte;
end

iic_led_interface: Light = Light{
	toggle = IIC_LED_toggle as (any^) as (fn(any^): word),
};

fn IIC_LED_init(self: IIC_LED^; address: byte)
	self^.address = address;
	self^.interface = iic_led_interface@;
end

fn IIC_LED_toggle(self: IIC_LED^): word
	return IIC_send(self^.port, 2);
end

fn my_app(light: Light^^)
	light^^.toggle(light as (any^));
end

fake_status: byte = 0;
fn IIC_send(port: word; cmd: byte): word
	fake_status = bnot(fake_status);
	return fake_status;
end

fn main()
	my_led: IIC_LED;
	IIC_LED_init(my_led@, 0x10);
	my_app(my_led@ as (Light^^));
end

