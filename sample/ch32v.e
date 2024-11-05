struct GPIO
	CFG_L			: word;			%% Port configuration register low
	CFG_H			: word;			%% Port configuration register high
	IN			: word;			%% Port input data register
	OUT			: word;			%% Port output data register
	BSH			: word;			%% Port set/reset register
	BC			: word;			%% Port reset register
	LOCK			: word;			%% Port configuration lock register
end

struct EXTI
	INTEN			: word;			%% Interrupt enable register
	EVEN			: word;			%% Event enable register
	RTEN			: word;			%% Rising edge trigger enable register
	FTEN			: word;			%% Falling edge trigger enable register
	SWIEV			: word;			%% Software interrupt event register
	INTF			: word;			%% Interrupt flag register
end

struct ADTM
	CTL1			: word;			%% Control register 1
	CTL2			: word;			%% Control register 2
	SMCFG			: word;			%% Slave mode configuration register
	DMAINTEN		: word;			%% DMA/interrupt enable register
	INTF			: word;			%% Interrupt flag register
	SWEVG			: word;			%% Event generation register
	CHCTL1			: word;			%% Compare/Capture control register 1
	CHCTL2			: word;			%% Compare/Capture control register 2
	CCEN			: word;			%% Compare/capture enable register
	CNT			: word;			%% Counter
	PSC			: word;			%% Prescaler
	ATRL			: word;			%% Auto-reload register
	RPTC			: word;			%% Repeat count register
	CHCV1			: word;			%% Compare/Capture register 1
	CHCV2			: word;			%% Compare/Capture register 2
	CHCV3			: word;			%% Compare/Capture register 3
	CHCV4			: word;			%% Compare/Capture register 4
	BDT			: word;			%% Break and deadband register
	DMACFG			: word;			%% DMA configuration register
	DMAAD			: word;			%% DMA address register in continuous mode
	AUX			: word;			%% Dual-edge capture register
end

struct GPTM
	CTL1			: word;			%% Control register 1
	CTL2			: word;			%% Control register 2
	SMCFG			: word;			%% Slave mode configuration register
	DMAINTEN		: word;			%% DMA/interrupt enable register
	INTF			: word;			%% Interrupt flag register
	SWEVG			: word;			%% Event generation register
	CHCTL1			: word;			%% Compare/Capture control register 1
	CHCTL2			: word;			%% Compare/Capture control register 2
	CCEN			: word;			%% Compare/capture enable register
	CNT			: word;			%% Counter
	PSC			: word;			%% Prescaler
	ATRL			: word;			%% Auto-reload register
	CHCV1			: word;			%% Compare/Capture register 1
	CHCV2			: word;			%% Compare/Capture register 2
	CHCV3			: word;			%% Compare/Capture register 3
	CHCV4			: word;			%% Compare/Capture register 4
	DMACFG			: word;			%% DMA configuration register
	DMAAD			: word;			%% DMA address register in continuous mode
	AUX			: word;			%% Dual-edge capture register
end

struct USART
	STATUS			: word;			%% Status register
	DATA			: word;			%% Data register
	BR			: word;			%% Baudrate register
	CTL1			: word;			%% Control register 1
	CTL2			: word;			%% Control register 2
	CTL3			: word;			%% Control register 3
	GP			: word;			%% Guard time and prescaler
	CTL4			: word;			%% Control register 4
end

struct ADC
	STATUS			: word;			%% Status register
	CTL1			: word;			%% Control register 1
	CTL2			: word;			%% Control register 2
	SAMPT1			: word;			%% Sample time configuration register 1
	SAMPT2			: word;			%% Sample time configuration register 2
	IOF1			: word;			%% Injected channel data offset register 1
	IOF2			: word;			%% Injected channel data offset register 2
	IOF3			: word;			%% Injected channel data offset register 3
	IOF4			: word;			%% Injected channel data offset register 4
	WDHT			: word;			%% Watchdog high threshold register
	WDLT			: word;			%% Watchdog low threshold register
	RSQ1			: word;			%% Regular channel sequence register 1
	RSQ2			: word;			%% Regular channel sequence register 2
	RSQ3			: word;			%% Regular channel sequence register 3
	ISQ			: word;			%% Injected channel sequence register
	IDATA1			: word;			%% Injected data register 1
	IDATA2			: word;			%% Injected data register 2
	IDATA3			: word;			%% Injected data register 3
	IDATA4			: word;			%% Injected data register 4
	RDATA			: word;			%% Regular data register
	AUX			: word;			%% Sample time register
end

struct DAC
	CTL			: word;			%% Control register
	SWT			: word;			%% Software trigger register
	R12BDH1			: word;			%% Channel1 right-aligned 12-bit data hold register
	L12BDH1			: word;			%% Channel1 left-aligned 12-bit data hold register
	R8BDH1			: word;			%% Channel1 right-aligned 8-bit data hold register
	R12BDH2			: word;			%% Channel2 right-aligned 12-bit data hold register
	L12BDH2			: word;			%% Channel2 left-aligned 12-bit data hold register
	R8BDH2			: word;			%% Channel2 right-aligned 8-bit data hold register
	RD12BDH			: word;			%% Dual channel right-aligned 12-bit data hold register
	LD12BDH			: word;			%% Dual channel left-aligned 12-bit data hold register
	RD8BDH			: word;			%% Dual channel right-aligned 8-bit data hold register
	DA1			: word;			%% Channel1 data output register
	DA2			: word;			%% Channel2 data output register
end

#define PWR_CTL			(0x4000_7000 as (word^))%% Power control register
#define PWR_CS			(0x4000_7004 as (word^))%% Power control/status register

#define RCC_CTL			(0x4002_1000 as (word^))%% Clock control register
#define RCC_CFG0		(0x4002_1004 as (word^))%% Clock configuration register 0
#define RCC_INT			(0x4002_1008 as (word^))%% Clock interrupt register
#define RCC_APB2RST		(0x4002_100C as (word^))%% PB2 peripheral reset register
#define RCC_APB1RST		(0x4002_1010 as (word^))%% PB1 peripheral reset register
#define RCC_AHBCEN		(0x4002_1014 as (word^))%% HB peripheral clock enable register
#define RCC_APB2CEN		(0x4002_1018 as (word^))%% PB2 peripheral clock enable register
#define RCC_APB1CEN		(0x4002_101C as (word^))%% PB1 peripheral clock enable register
#define RCC_BDCTL		(0x4002_1020 as (word^))%% Backup domain control register
#define RCC_RSTSCK		(0x4002_1024 as (word^))%% Control/status register
#define RCC_AHBRST		(0x4002_1028 as (word^))%% HB peripheral reset register
#define RCC_CFG2		(0x4002_102C as (word^))%% Clock configuration register 2

#define HSE_CAL_CTL		(0x4002_202C as (word^))%% HSE crystal oscillator calibration control register

#define AFIO_EC			(0x4001_0000 as (word^))%% Event control register
#define AFIO_PCF1		(0x4001_0004 as (word^))%% Remap register 1
#define AFIO_EXTICx		(0x4001_0008 as (word^))%% External interrupt configuration registers (4)
#define AFIO_PCF2		(0x4001_001C as (word^))%% Remap register 2

#define PFIC_ISx		(0xE000_E000 as (word^))%% Interrupt enable status registers (4)
#define PFIC_IPx		(0xE000_E020 as (word^))%% Interrupt pending status registers (4)
#define PFIC_ITHRESD		(0xE000_E040 as (word^))%% Interrupt priority threshold configuration register
#define PFIC_CFG		(0xE000_E048 as (word^))%% Interrupt configuration register
#define PFIC_GIS		(0xE000_E04C as (word^))%% Interrupt global status register
#define PFIC_VTFID		(0xE000_E050 as (word^))%% VTF interrupt ID configuration register
#define PFIC_VTFADDRx		(0xE000_E060 as (word^))%% VTF interrupt0 offset address registers (4)
#define PFIC_IENx		(0xE000_E100 as (word^))%% Interrupt enable set registers (4)
#define PFIC_IREx		(0xE000_E180 as (word^))%% Interrupt enable clear registers (4)
#define PFIC_IPSx		(0xE000_E200 as (word^))%% Interrupt pending set registers (4)
#define PFIC_IPRx		(0xE000_E280 as (word^))%% Interrupt pending clear registers (4)
#define PFIC_IACTx		(0xE000_E300 as (word^))%% Interrupt activation registers (4)
#define PFIC_IPRIORx		(0xE000_E400 as (word^))%% Interrupt priority configuration registers (64)
#define PFIC_SCTL		(0xE000_ED10 as (word^))%% System control register

#define	GPIOA			(0x4001_0800 as (GPIO^))
#define	GPIOB			(0x4001_0C00 as (GPIO^))
#define	GPIOC			(0x4001_1000 as (GPIO^))
#define	GPIOD			(0x4001_1400 as (GPIO^))
#define	GPIOE			(0x4001_1800 as (GPIO^))

#define EXTI			(0x4001_0400 as (EXTI^))

#define TIM1			(0x4001_2C00 as (ADTM^))
#define TIM2			(0x4000_0000 as (GPTM^))
#define TIM3			(0x4000_0400 as (GPTM^))
#define TIM4			(0x4000_0800 as (GPTM^))
#define TIM5			(0x4000_0C00 as (GPTM^))

#define USART1			(0x4001_3800 as (USART^))
#define USART2			(0x4000_4400 as (USART^))
#define USART3			(0x4000_4800 as (USART^))
#define USART4			(0x4000_4C00 as (USART^))
#define USART5			(0x4000_5000 as (USART^))
#define USART6			(0x4000_1800 as (USART^))
#define USART7			(0x4000_1C00 as (USART^))
#define USART8			(0x4000_2000 as (USART^))

#define ADC1			(0x4001_2400 as (ADC^))
#define ADC2			(0x4001_2800 as (ADC^))

#define DAC			(0x4000_7400 as (DAC^))

