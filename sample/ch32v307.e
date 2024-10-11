unused_string3: byte^ = "this string is unused, just for testing linking.";

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

	%% We use the internal 8MHz as system clock.
	tim2^.ATRL = 8000;
	%tim2^.PSC = 9; % 10ms
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

