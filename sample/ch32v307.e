unused_string3: byte^ = "this string is unused, just for testing linking.";
blah: word = 1234;

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

