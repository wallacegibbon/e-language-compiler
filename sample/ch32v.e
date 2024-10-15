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

struct ADTM
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
	RPTC			: word; %% Repeat count register
	CH1CV			: word; %% Compare/Capture register1
	CH2CV			: word; %% Compare/Capture register2
	CH3CV			: word; %% Compare/Capture register3
	CH4CV			: word; %% Compare/Capture register4
	BDT			: word; %% Break and deadband register
	DMACFG			: word; %% DMA configuration register
	DMAAD			: word; %% DMA address register in continuous mode
	AUX			: word; %% Dual-edge capture register
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

GPIOD		: GPIO^ = 0x4001_1400 as (GPIO^);
EXTI		: EXTI^ = 0x4001_0400 as (EXTI^);
TIM1		: ADTM^ = 0x4001_2C00 as (ADTM^);
TIM2		: GPTM^ = 0x4000_0000 as (GPTM^);
TIM3		: GPTM^ = 0x4000_0400 as (GPTM^);
TIM4		: GPTM^ = 0x4000_0800 as (GPTM^);
TIM5		: GPTM^ = 0x4000_0C00 as (GPTM^);

