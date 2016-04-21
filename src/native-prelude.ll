@.error_msg = constant [16 x i8] c"Error occurred\0A\00"
@.scanf_format_int = constant [6 x i8] c"%lld\0A\00"
@.printf_format_int = constant [6 x i8] c"%lld\0A\00"

declare void @exit(i32)
declare i32 @scanf(i8*, ...)
declare i32 @printf(i8*, ...)

define void @stdExit(i32 %status)
{
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @.error_msg, i32 0, i32 0))
    call void @exit(i32 %status)
    ret void
}

define i64 @stdReadInt() {
    %addr = alloca i64
    %r = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([6 x i8]* @.scanf_format_int, i32 0, i32 0), i64* %addr)
    %good = icmp eq i32 %r, 1
    br i1 %good, label %stdReadInt.success, label %stdReadInt.failure
stdReadInt.success:
    %res = load i64* %addr
    ret i64 %res
stdReadInt.failure:
    call void @stdExit(i32 -1)
    ret i64 -1
}

define void @stdWriteInt(i64 %a) {
    %r = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.printf_format_int, i32 0, i32 0), i64 %a)
    %good = icmp sgt i32 %r, 0
    br i1 %good, label %stdWriteInt.success, label %stdWriteInt.failure
stdWriteInt.success:
    ret void
stdWriteInt.failure:
    call void @stdExit(i32 -2)
    ret void
}
