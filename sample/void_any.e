fn main(): void
	%% In E language, `any` type can only be used as pointer. (like `void *` in C language)
	%a: any;
	b: any^;

	%% In E language, `void` type can only be used as return type, and can not be pointer.
	%c: void;
	%d: void^;
end
