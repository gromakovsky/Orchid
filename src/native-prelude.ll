@.printf_format_int = constant [3 x i8] c"%d\00"

declare i32 @printf(i8*, ...)

define i64 @stdReadInt() {
    ret i64 42
}

define void @stdWriteInt(i64 %a) {
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.printf_format_int, i32 0, i32 0), i64 %a)
    ret void
}
