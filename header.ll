; ModuleID = 'header.cpp'
source_filename = "header.cpp"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.11.0"

%class.hamt = type { [7 x %class.KV], i64 }
%class.KV = type { %"union.KV<key, value, 0>::Key", %"union.KV<key, value, 0>::Val" }
%"union.KV<key, value, 0>::Key" = type { i64 }
%"union.KV<key, value, 0>::Val" = type { %class.KV.0* }
%class.KV.0 = type { %"union.KV<key, value, 1>::Key", %"union.KV<key, value, 1>::Val" }
%"union.KV<key, value, 1>::Key" = type { i64 }
%"union.KV<key, value, 1>::Val" = type { %class.KV.1* }
%class.KV.1 = type { %"union.KV<key, value, 2>::Key", %"union.KV<key, value, 2>::Val" }
%"union.KV<key, value, 2>::Key" = type { i64 }
%"union.KV<key, value, 2>::Val" = type { %class.KV.2* }
%class.KV.2 = type { %"union.KV<key, value, 3>::Key", %"union.KV<key, value, 3>::Val" }
%"union.KV<key, value, 3>::Key" = type { i64 }
%"union.KV<key, value, 3>::Val" = type { %class.KV.3* }
%class.KV.3 = type { %"union.KV<key, value, 4>::Key", %"union.KV<key, value, 4>::Val" }
%"union.KV<key, value, 4>::Key" = type { i64 }
%"union.KV<key, value, 4>::Val" = type { %class.KV.4* }
%class.KV.4 = type { %"union.KV<key, value, 5>::Key", %"union.KV<key, value, 5>::Val" }
%"union.KV<key, value, 5>::Key" = type { i64 }
%"union.KV<key, value, 5>::Val" = type { %class.KV.5* }
%class.KV.5 = type { %"union.KV<key, value, 6>::Key", %"union.KV<key, value, 6>::Val" }
%"union.KV<key, value, 6>::Key" = type { i64 }
%"union.KV<key, value, 6>::Val" = type { %class.KV.6* }
%class.KV.6 = type { %"union.KV<key, value, 7>::Key", %"union.KV<key, value, 7>::Val" }
%"union.KV<key, value, 7>::Key" = type { i64 }
%"union.KV<key, value, 7>::Val" = type { %class.KV.7* }
%class.KV.7 = type { %"union.KV<key, value, 8>::Key", %"union.KV<key, value, 8>::Val" }
%"union.KV<key, value, 8>::Key" = type { i64 }
%"union.KV<key, value, 8>::Val" = type { %class.KV.8* }
%class.KV.8 = type { %"union.KV<key, value, 9>::Key", %"union.KV<key, value, 9>::Val" }
%"union.KV<key, value, 9>::Key" = type { i64 }
%"union.KV<key, value, 9>::Val" = type { %class.KV.9* }
%class.KV.9 = type { %"union.KV<key, value, 10>::Key", %"union.KV<key, value, 10>::Val" }
%"union.KV<key, value, 10>::Key" = type { i64 }
%"union.KV<key, value, 10>::Val" = type { %class.LL* }
%class.LL = type { %class.key*, %class.value*, %class.LL* }
%class.key = type { i64 }
%class.value = type { i64 }

@.str = private unnamed_addr constant [25 x i8] c"library run-time error: \00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.3 = private unnamed_addr constant [6 x i8] c"%llu\0A\00", align 1
@.str.4 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.5 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.6 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.7 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.8 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.9 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.10 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.11 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.12 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.13 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.14 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.15 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.16 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.17 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.18 = private unnamed_addr constant [37 x i8] c"(print.. v); unrecognized value %llu\00", align 1
@.str.19 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.20 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.21 = private unnamed_addr constant [4 x i8] c"%ld\00", align 1
@.str.22 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.23 = private unnamed_addr constant [35 x i8] c"(print v); unrecognized value %llu\00", align 1
@.str.24 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.25 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.26 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.27 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.28 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.29 = private unnamed_addr constant [49 x i8] c"second argument to vector-set must be an integer\00", align 1
@.str.30 = private unnamed_addr constant [48 x i8] c"first argument to vector-set must be an integer\00", align 1
@.str.31 = private unnamed_addr constant [42 x i8] c"first argument to hash-ref must be a hash\00", align 1
@.str.32 = private unnamed_addr constant [42 x i8] c"first argument to hash-set must be a hash\00", align 1
@.str.33 = private unnamed_addr constant [45 x i8] c"first argument to hash-remove must be a hash\00", align 1
@.str.34 = private unnamed_addr constant [47 x i8] c"first argument to hash-has-key? must be a hash\00", align 1
@.str.35 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.37 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.38 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.39 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.40 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.41 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.42 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.43 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.44 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.45 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.46 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.47 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.48 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1

; Function Attrs: ssp uwtable
define i64* @alloc(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 %3, i64 8)
  %5 = extractvalue { i64, i1 } %4, 1
  %6 = extractvalue { i64, i1 } %4, 0
  %7 = select i1 %5, i64 -1, i64 %6
  %8 = call noalias i8* @_Znam(i64 %7) #8
  %9 = bitcast i8* %8 to i64*
  ret i64* %9
}

; Function Attrs: nounwind readnone
declare { i64, i1 } @llvm.umul.with.overflow.i64(i64, i64) #1

; Function Attrs: nobuiltin
declare noalias i8* @_Znam(i64) #2

; Function Attrs: ssp uwtable
define void @fatal_err(i8*) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str, i32 0, i32 0))
  %4 = load i8*, i8** %2, align 8
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %4)
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #9
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #3

; Function Attrs: noreturn
declare void @exit(i32) #4

; Function Attrs: ssp uwtable
define void @print_u64(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.3, i32 0, i32 0), i64 %3)
  ret void
}

; Function Attrs: ssp uwtable
define i64 @expect_args0(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp ne i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.4, i32 0, i32 0))
  br label %6

; <label>:6                                       ; preds = %5, %1
  ret i64 0
}

; Function Attrs: ssp uwtable
define i64 @expect_args1(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 1
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.5, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %1
  %9 = load i64, i64* %2, align 8
  %10 = and i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  store i64* %11, i64** %3, align 8
  %12 = load i64*, i64** %3, align 8
  %13 = getelementptr inbounds i64, i64* %12, i64 1
  %14 = load i64, i64* %13, align 8
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %16, label %17

; <label>:16                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.6, i32 0, i32 0))
  br label %17

; <label>:17                                      ; preds = %16, %8
  %18 = load i64*, i64** %3, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: ssp uwtable
define i64 @expect_cons(i64, i64*) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64* %1, i64** %4, align 8
  %6 = load i64, i64* %3, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 1
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.7, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %9, %2
  %11 = load i64, i64* %3, align 8
  %12 = and i64 %11, -8
  %13 = inttoptr i64 %12 to i64*
  store i64* %13, i64** %5, align 8
  %14 = load i64*, i64** %5, align 8
  %15 = getelementptr inbounds i64, i64* %14, i64 1
  %16 = load i64, i64* %15, align 8
  %17 = load i64*, i64** %4, align 8
  store i64 %16, i64* %17, align 8
  %18 = load i64*, i64** %5, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: ssp uwtable
define i64 @expect_other(i64, i64*) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64* %1, i64** %4, align 8
  %6 = load i64, i64* %3, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 6
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.8, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %9, %2
  %11 = load i64, i64* %3, align 8
  %12 = and i64 %11, -8
  %13 = inttoptr i64 %12 to i64*
  store i64* %13, i64** %5, align 8
  %14 = load i64*, i64** %5, align 8
  %15 = getelementptr inbounds i64, i64* %14, i64 1
  %16 = load i64, i64* %15, align 8
  %17 = load i64*, i64** %4, align 8
  store i64 %16, i64* %17, align 8
  %18 = load i64*, i64** %5, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_int(i64) #5 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = shl i64 %3, 32
  %5 = or i64 %4, 2
  ret i64 %5
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_void() #5 {
  ret i64 39
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_null() #5 {
  ret i64 0
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_true() #5 {
  ret i64 31
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_false() #5 {
  ret i64 15
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_string(i8*) #5 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = ptrtoint i8* %3 to i64
  %5 = or i64 %4, 3
  ret i64 %5
}

; Function Attrs: nounwind ssp uwtable
define i64 @const_init_symbol(i8*) #5 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = ptrtoint i8* %3 to i64
  %5 = or i64 %4, 4
  ret i64 %5
}

; Function Attrs: ssp uwtable
define i64 @prim_print_aux(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %9, label %11

; <label>:9                                       ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.9, i32 0, i32 0))
  br label %113

; <label>:11                                      ; preds = %1
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, 7
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %15, label %17

; <label>:15                                      ; preds = %11
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.10, i32 0, i32 0))
  br label %112

; <label>:17                                      ; preds = %11
  %18 = load i64, i64* %2, align 8
  %19 = and i64 %18, 7
  %20 = icmp eq i64 %19, 1
  br i1 %20, label %21, label %36

; <label>:21                                      ; preds = %17
  %22 = load i64, i64* %2, align 8
  %23 = and i64 %22, -8
  %24 = inttoptr i64 %23 to i64*
  store i64* %24, i64** %3, align 8
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.11, i32 0, i32 0))
  %26 = load i64*, i64** %3, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 0
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.12, i32 0, i32 0))
  %31 = load i64*, i64** %3, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 1
  %33 = load i64, i64* %32, align 8
  %34 = call i64 @prim_print_aux(i64 %33)
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %111

; <label>:36                                      ; preds = %17
  %37 = load i64, i64* %2, align 8
  %38 = and i64 %37, 7
  %39 = icmp eq i64 %38, 2
  br i1 %39, label %40, label %45

; <label>:40                                      ; preds = %36
  %41 = load i64, i64* %2, align 8
  %42 = lshr i64 %41, 32
  %43 = trunc i64 %42 to i32
  %44 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.14, i32 0, i32 0), i32 %43)
  br label %110

; <label>:45                                      ; preds = %36
  %46 = load i64, i64* %2, align 8
  %47 = and i64 %46, 7
  %48 = icmp eq i64 %47, 3
  br i1 %48, label %49, label %54

; <label>:49                                      ; preds = %45
  %50 = load i64, i64* %2, align 8
  %51 = and i64 %50, -8
  %52 = inttoptr i64 %51 to i8*
  %53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.15, i32 0, i32 0), i8* %52)
  br label %109

; <label>:54                                      ; preds = %45
  %55 = load i64, i64* %2, align 8
  %56 = and i64 %55, 7
  %57 = icmp eq i64 %56, 4
  br i1 %57, label %58, label %63

; <label>:58                                      ; preds = %54
  %59 = load i64, i64* %2, align 8
  %60 = and i64 %59, -8
  %61 = inttoptr i64 %60 to i8*
  %62 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %61)
  br label %108

; <label>:63                                      ; preds = %54
  %64 = load i64, i64* %2, align 8
  %65 = and i64 %64, 7
  %66 = icmp eq i64 %65, 6
  br i1 %66, label %67, label %104

; <label>:67                                      ; preds = %63
  %68 = load i64, i64* %2, align 8
  %69 = and i64 %68, -8
  %70 = inttoptr i64 %69 to i64*
  %71 = getelementptr inbounds i64, i64* %70, i64 0
  %72 = load i64, i64* %71, align 8
  %73 = and i64 %72, 7
  %74 = icmp eq i64 1, %73
  br i1 %74, label %75, label %104

; <label>:75                                      ; preds = %67
  %76 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0))
  %77 = load i64, i64* %2, align 8
  %78 = and i64 %77, -8
  %79 = inttoptr i64 %78 to i64*
  store i64* %79, i64** %4, align 8
  %80 = load i64*, i64** %4, align 8
  %81 = getelementptr inbounds i64, i64* %80, i64 0
  %82 = load i64, i64* %81, align 8
  %83 = lshr i64 %82, 3
  store i64 %83, i64* %5, align 8
  %84 = load i64*, i64** %4, align 8
  %85 = getelementptr inbounds i64, i64* %84, i64 1
  %86 = load i64, i64* %85, align 8
  %87 = call i64 @prim_print_aux(i64 %86)
  store i64 2, i64* %6, align 8
  br label %88

; <label>:88                                      ; preds = %99, %75
  %89 = load i64, i64* %6, align 8
  %90 = load i64, i64* %5, align 8
  %91 = icmp ule i64 %89, %90
  br i1 %91, label %92, label %102

; <label>:92                                      ; preds = %88
  %93 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.17, i32 0, i32 0))
  %94 = load i64, i64* %6, align 8
  %95 = load i64*, i64** %4, align 8
  %96 = getelementptr inbounds i64, i64* %95, i64 %94
  %97 = load i64, i64* %96, align 8
  %98 = call i64 @prim_print_aux(i64 %97)
  br label %99

; <label>:99                                      ; preds = %92
  %100 = load i64, i64* %6, align 8
  %101 = add i64 %100, 1
  store i64 %101, i64* %6, align 8
  br label %88

; <label>:102                                     ; preds = %88
  %103 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %107

; <label>:104                                     ; preds = %67, %63
  %105 = load i64, i64* %2, align 8
  %106 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.18, i32 0, i32 0), i64 %105)
  br label %107

; <label>:107                                     ; preds = %104, %102
  br label %108

; <label>:108                                     ; preds = %107, %58
  br label %109

; <label>:109                                     ; preds = %108, %49
  br label %110

; <label>:110                                     ; preds = %109, %40
  br label %111

; <label>:111                                     ; preds = %110, %21
  br label %112

; <label>:112                                     ; preds = %111, %15
  br label %113

; <label>:113                                     ; preds = %112, %9
  ret i64 39
}

; Function Attrs: ssp uwtable
define i64 @prim_print(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %9, label %11

; <label>:9                                       ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.19, i32 0, i32 0))
  br label %112

; <label>:11                                      ; preds = %1
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, 7
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %15, label %17

; <label>:15                                      ; preds = %11
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.10, i32 0, i32 0))
  br label %111

; <label>:17                                      ; preds = %11
  %18 = load i64, i64* %2, align 8
  %19 = and i64 %18, 7
  %20 = icmp eq i64 %19, 1
  br i1 %20, label %21, label %36

; <label>:21                                      ; preds = %17
  %22 = load i64, i64* %2, align 8
  %23 = and i64 %22, -8
  %24 = inttoptr i64 %23 to i64*
  store i64* %24, i64** %3, align 8
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.20, i32 0, i32 0))
  %26 = load i64*, i64** %3, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 0
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.12, i32 0, i32 0))
  %31 = load i64*, i64** %3, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 1
  %33 = load i64, i64* %32, align 8
  %34 = call i64 @prim_print_aux(i64 %33)
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %110

; <label>:36                                      ; preds = %17
  %37 = load i64, i64* %2, align 8
  %38 = and i64 %37, 7
  %39 = icmp eq i64 %38, 2
  br i1 %39, label %40, label %44

; <label>:40                                      ; preds = %36
  %41 = load i64, i64* %2, align 8
  %42 = lshr i64 %41, 32
  %43 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.21, i32 0, i32 0), i64 %42)
  br label %109

; <label>:44                                      ; preds = %36
  %45 = load i64, i64* %2, align 8
  %46 = and i64 %45, 7
  %47 = icmp eq i64 %46, 3
  br i1 %47, label %48, label %53

; <label>:48                                      ; preds = %44
  %49 = load i64, i64* %2, align 8
  %50 = and i64 %49, -8
  %51 = inttoptr i64 %50 to i8*
  %52 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.15, i32 0, i32 0), i8* %51)
  br label %108

; <label>:53                                      ; preds = %44
  %54 = load i64, i64* %2, align 8
  %55 = and i64 %54, 7
  %56 = icmp eq i64 %55, 4
  br i1 %56, label %57, label %62

; <label>:57                                      ; preds = %53
  %58 = load i64, i64* %2, align 8
  %59 = and i64 %58, -8
  %60 = inttoptr i64 %59 to i8*
  %61 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.22, i32 0, i32 0), i8* %60)
  br label %107

; <label>:62                                      ; preds = %53
  %63 = load i64, i64* %2, align 8
  %64 = and i64 %63, 7
  %65 = icmp eq i64 %64, 6
  br i1 %65, label %66, label %103

; <label>:66                                      ; preds = %62
  %67 = load i64, i64* %2, align 8
  %68 = and i64 %67, -8
  %69 = inttoptr i64 %68 to i64*
  %70 = getelementptr inbounds i64, i64* %69, i64 0
  %71 = load i64, i64* %70, align 8
  %72 = and i64 %71, 7
  %73 = icmp eq i64 1, %72
  br i1 %73, label %74, label %103

; <label>:74                                      ; preds = %66
  %75 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0))
  %76 = load i64, i64* %2, align 8
  %77 = and i64 %76, -8
  %78 = inttoptr i64 %77 to i64*
  store i64* %78, i64** %4, align 8
  %79 = load i64*, i64** %4, align 8
  %80 = getelementptr inbounds i64, i64* %79, i64 0
  %81 = load i64, i64* %80, align 8
  %82 = lshr i64 %81, 3
  store i64 %82, i64* %5, align 8
  %83 = load i64*, i64** %4, align 8
  %84 = getelementptr inbounds i64, i64* %83, i64 1
  %85 = load i64, i64* %84, align 8
  %86 = call i64 @prim_print(i64 %85)
  store i64 2, i64* %6, align 8
  br label %87

; <label>:87                                      ; preds = %98, %74
  %88 = load i64, i64* %6, align 8
  %89 = load i64, i64* %5, align 8
  %90 = icmp ule i64 %88, %89
  br i1 %90, label %91, label %101

; <label>:91                                      ; preds = %87
  %92 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.17, i32 0, i32 0))
  %93 = load i64, i64* %6, align 8
  %94 = load i64*, i64** %4, align 8
  %95 = getelementptr inbounds i64, i64* %94, i64 %93
  %96 = load i64, i64* %95, align 8
  %97 = call i64 @prim_print(i64 %96)
  br label %98

; <label>:98                                      ; preds = %91
  %99 = load i64, i64* %6, align 8
  %100 = add i64 %99, 1
  store i64 %100, i64* %6, align 8
  br label %87

; <label>:101                                     ; preds = %87
  %102 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %106

; <label>:103                                     ; preds = %66, %62
  %104 = load i64, i64* %2, align 8
  %105 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.23, i32 0, i32 0), i64 %104)
  br label %106

; <label>:106                                     ; preds = %103, %101
  br label %107

; <label>:107                                     ; preds = %106, %57
  br label %108

; <label>:108                                     ; preds = %107, %48
  br label %109

; <label>:109                                     ; preds = %108, %40
  br label %110

; <label>:110                                     ; preds = %109, %21
  br label %111

; <label>:111                                     ; preds = %110, %15
  br label %112

; <label>:112                                     ; preds = %111, %9
  ret i64 39
}

; Function Attrs: ssp uwtable
define i64 @applyprim_print(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_print(i64 %6)
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @prim_halt(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = call i64 @prim_print(i64 %4)
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 0) #9
  unreachable
                                                  ; No predecessors!
  %8 = load i64, i64* %2, align 8
  ret i64 %8
}

; Function Attrs: ssp uwtable
define i64 @applyprim_vector(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = call noalias i8* @_Znam(i64 2048) #8
  %8 = bitcast i8* %7 to i64*
  store i64* %8, i64** %3, align 8
  store i64 0, i64* %4, align 8
  br label %9

; <label>:9                                       ; preds = %18, %1
  %10 = load i64, i64* %2, align 8
  %11 = and i64 %10, 7
  %12 = icmp eq i64 %11, 1
  br i1 %12, label %13, label %16

; <label>:13                                      ; preds = %9
  %14 = load i64, i64* %4, align 8
  %15 = icmp ult i64 %14, 256
  br label %16

; <label>:16                                      ; preds = %13, %9
  %17 = phi i1 [ false, %9 ], [ %15, %13 ]
  br i1 %17, label %18, label %25

; <label>:18                                      ; preds = %16
  %19 = load i64, i64* %2, align 8
  %20 = call i64 @expect_cons(i64 %19, i64* %2)
  %21 = load i64, i64* %4, align 8
  %22 = add i64 %21, 1
  store i64 %22, i64* %4, align 8
  %23 = load i64*, i64** %3, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 %21
  store i64 %20, i64* %24, align 8
  br label %9

; <label>:25                                      ; preds = %16
  %26 = load i64, i64* %4, align 8
  %27 = add i64 %26, 1
  %28 = call i64* @alloc(i64 %27)
  store i64* %28, i64** %5, align 8
  %29 = load i64, i64* %4, align 8
  %30 = shl i64 %29, 3
  %31 = or i64 %30, 1
  %32 = load i64*, i64** %5, align 8
  %33 = getelementptr inbounds i64, i64* %32, i64 0
  store i64 %31, i64* %33, align 8
  store i64 1, i64* %6, align 8
  br label %34

; <label>:34                                      ; preds = %47, %25
  %35 = load i64, i64* %6, align 8
  %36 = load i64, i64* %4, align 8
  %37 = icmp ule i64 %35, %36
  br i1 %37, label %38, label %50

; <label>:38                                      ; preds = %34
  %39 = load i64, i64* %6, align 8
  %40 = sub i64 %39, 1
  %41 = load i64*, i64** %3, align 8
  %42 = getelementptr inbounds i64, i64* %41, i64 %40
  %43 = load i64, i64* %42, align 8
  %44 = load i64, i64* %6, align 8
  %45 = load i64*, i64** %5, align 8
  %46 = getelementptr inbounds i64, i64* %45, i64 %44
  store i64 %43, i64* %46, align 8
  br label %47

; <label>:47                                      ; preds = %38
  %48 = load i64, i64* %6, align 8
  %49 = add i64 %48, 1
  store i64 %49, i64* %6, align 8
  br label %34

; <label>:50                                      ; preds = %34
  %51 = load i64*, i64** %3, align 8
  %52 = icmp eq i64* %51, null
  br i1 %52, label %55, label %53

; <label>:53                                      ; preds = %50
  %54 = bitcast i64* %51 to i8*
  call void @_ZdaPv(i8* %54) #10
  br label %55

; <label>:55                                      ; preds = %53, %50
  %56 = load i64*, i64** %5, align 8
  %57 = ptrtoint i64* %56 to i64
  %58 = or i64 %57, 6
  ret i64 %58
}

; Function Attrs: nobuiltin nounwind
declare void @_ZdaPv(i8*) #6

; Function Attrs: ssp uwtable
define i64 @prim_make_45vector(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64*, align 8
  %7 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.24, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %2
  %13 = load i64, i64* %3, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  store i64 %15, i64* %5, align 8
  %16 = load i64, i64* %5, align 8
  %17 = mul i64 %16, 8
  %18 = add i64 2, %17
  %19 = call i64* @alloc(i64 %18)
  store i64* %19, i64** %6, align 8
  %20 = load i64*, i64** %6, align 8
  %21 = getelementptr inbounds i64, i64* %20, i64 0
  store i64 1, i64* %21, align 8
  %22 = load i64, i64* %3, align 8
  %23 = load i64*, i64** %6, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 1
  store i64 %22, i64* %24, align 8
  store i64 2, i64* %7, align 8
  br label %25

; <label>:25                                      ; preds = %35, %12
  %26 = load i64, i64* %7, align 8
  %27 = load i64, i64* %5, align 8
  %28 = add i64 %27, 1
  %29 = icmp ule i64 %26, %28
  br i1 %29, label %30, label %38

; <label>:30                                      ; preds = %25
  %31 = load i64, i64* %4, align 8
  %32 = load i64, i64* %7, align 8
  %33 = load i64*, i64** %6, align 8
  %34 = getelementptr inbounds i64, i64* %33, i64 %32
  store i64 %31, i64* %34, align 8
  br label %35

; <label>:35                                      ; preds = %30
  %36 = load i64, i64* %7, align 8
  %37 = add i64 %36, 1
  store i64 %37, i64* %7, align 8
  br label %25

; <label>:38                                      ; preds = %25
  %39 = load i64*, i64** %6, align 8
  %40 = ptrtoint i64* %39 to i64
  %41 = or i64 %40, 6
  ret i64 %41
}

; Function Attrs: ssp uwtable
define i64 @applyprim_make_45vector(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_make_45vector(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: ssp uwtable
define i64 @prim_vector_45length(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 6
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.26, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %1
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  %11 = getelementptr inbounds i64, i64* %10, i64 0
  %12 = load i64, i64* %11, align 8
  %13 = and i64 %12, 7
  %14 = icmp ne i64 %13, 1
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.27, i32 0, i32 0))
  br label %16

; <label>:16                                      ; preds = %15, %7
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = inttoptr i64 %18 to i64*
  %20 = getelementptr inbounds i64, i64* %19, i64 1
  %21 = load i64, i64* %20, align 8
  ret i64 %21
}

; Function Attrs: ssp uwtable
define i64 @prim_vector_45ref(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %4, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.28, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %2
  %10 = load i64, i64* %3, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 6
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.26, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = inttoptr i64 %16 to i64*
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, 7
  %21 = icmp ne i64 %20, 1
  br i1 %21, label %22, label %23

; <label>:22                                      ; preds = %14
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.27, i32 0, i32 0))
  br label %23

; <label>:23                                      ; preds = %22, %14
  %24 = load i64, i64* %4, align 8
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = add nsw i64 2, %26
  %28 = load i64, i64* %3, align 8
  %29 = and i64 %28, -8
  %30 = inttoptr i64 %29 to i64*
  %31 = getelementptr inbounds i64, i64* %30, i64 %27
  %32 = load i64, i64* %31, align 8
  ret i64 %32
}

; Function Attrs: ssp uwtable
define i64 @applyprim_vector_45ref(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_vector_45ref(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: ssp uwtable
define i64 @prim_vector_45set_33(i64, i64, i64) #0 {
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  store i64 %2, i64* %6, align 8
  %7 = load i64, i64* %5, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 2
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %3
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.29, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %3
  %12 = load i64, i64* %4, align 8
  %13 = and i64 %12, 7
  %14 = icmp ne i64 %13, 6
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %11
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.30, i32 0, i32 0))
  br label %16

; <label>:16                                      ; preds = %15, %11
  %17 = load i64, i64* %4, align 8
  %18 = and i64 %17, -8
  %19 = inttoptr i64 %18 to i64*
  %20 = getelementptr inbounds i64, i64* %19, i64 0
  %21 = load i64, i64* %20, align 8
  %22 = and i64 %21, 7
  %23 = icmp ne i64 %22, 1
  br i1 %23, label %24, label %25

; <label>:24                                      ; preds = %16
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.27, i32 0, i32 0))
  br label %25

; <label>:25                                      ; preds = %24, %16
  %26 = load i64, i64* %6, align 8
  %27 = load i64, i64* %5, align 8
  %28 = and i64 %27, -8
  %29 = lshr i64 %28, 32
  %30 = add nsw i64 2, %29
  %31 = load i64, i64* %4, align 8
  %32 = and i64 %31, -8
  %33 = inttoptr i64 %32 to i64*
  %34 = getelementptr inbounds i64, i64* %33, i64 %30
  store i64 %26, i64* %34, align 8
  ret i64 39
}

; Function Attrs: ssp uwtable
define i64 @applyprim_vector_45set_33(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = call i64 @expect_cons(i64 %7, i64* %3)
  store i64 %8, i64* %4, align 8
  %9 = load i64, i64* %3, align 8
  %10 = call i64 @expect_cons(i64 %9, i64* %3)
  store i64 %10, i64* %5, align 8
  %11 = load i64, i64* %3, align 8
  %12 = call i64 @expect_cons(i64 %11, i64* %3)
  store i64 %12, i64* %6, align 8
  %13 = load i64, i64* %3, align 8
  %14 = icmp ne i64 %13, 0
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %16

; <label>:16                                      ; preds = %15, %1
  %17 = load i64, i64* %4, align 8
  %18 = load i64, i64* %5, align 8
  %19 = load i64, i64* %6, align 8
  %20 = call i64 @prim_vector_45set_33(i64 %17, i64 %18, i64 %19)
  ret i64 %20
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_void() #5 {
  ret i64 39
}

; Function Attrs: ssp uwtable
define i64 @prim_hash() #0 {
  %1 = alloca %class.hamt*, align 8
  %2 = call i8* @malloc(i64 120)
  %3 = bitcast i8* %2 to %class.hamt*
  %4 = bitcast %class.hamt* %3 to i8*
  %5 = bitcast i8* %4 to %class.hamt*
  call void @_ZN4hamtI3key5valueEC1Ev(%class.hamt* %5)
  store %class.hamt* %5, %class.hamt** %1, align 8
  %6 = load %class.hamt*, %class.hamt** %1, align 8
  %7 = ptrtoint %class.hamt* %6 to i64
  %8 = or i64 %7, 6
  ret i64 %8
}

declare i8* @malloc(i64) #3

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN4hamtI3key5valueEC1Ev(%class.hamt*) unnamed_addr #0 align 2 {
  %2 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %2, align 8
  %3 = load %class.hamt*, %class.hamt** %2, align 8
  call void @_ZN4hamtI3key5valueEC2Ev(%class.hamt* %3)
  ret void
}

; Function Attrs: ssp uwtable
define i64 @prim_hash_45ref(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca %class.value*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 6
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.31, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %2
  %13 = load i64, i64* %3, align 8
  %14 = and i64 %13, -8
  %15 = inttoptr i64 %14 to i64*
  %16 = bitcast i64* %15 to %class.hamt*
  store %class.hamt* %16, %class.hamt** %5, align 8
  %17 = call i8* @malloc(i64 8)
  %18 = bitcast i8* %17 to %class.key*
  %19 = bitcast %class.key* %18 to i8*
  %20 = bitcast i8* %19 to %class.key*
  %21 = load i64, i64* %4, align 8
  call void @_ZN3keyC1Ey(%class.key* %20, i64 %21)
  store %class.key* %20, %class.key** %6, align 8
  %22 = load %class.hamt*, %class.hamt** %5, align 8
  %23 = load %class.key*, %class.key** %6, align 8
  %24 = call %class.value* @_ZNK4hamtI3key5valueE3getEPKS0_(%class.hamt* %22, %class.key* %23)
  store %class.value* %24, %class.value** %7, align 8
  %25 = load %class.value*, %class.value** %7, align 8
  %26 = getelementptr inbounds %class.value, %class.value* %25, i32 0, i32 0
  %27 = load i64, i64* %26, align 8
  ret i64 %27
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN3keyC1Ey(%class.key*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca i64, align 8
  store %class.key* %0, %class.key** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %class.key*, %class.key** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN3keyC2Ey(%class.key* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZNK4hamtI3key5valueE3getEPKS0_(%class.hamt*, %class.key*) #0 align 2 {
  %3 = alloca %class.value*, align 8
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %8 = load %class.hamt*, %class.hamt** %4, align 8
  %9 = load %class.key*, %class.key** %5, align 8
  %10 = call i64 @_ZNK3key4hashEv(%class.key* %9)
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %6, align 8
  %12 = and i64 %11, 15
  %13 = urem i64 %12, 7
  store i64 %13, i64* %7, align 8
  %14 = load i64, i64* %7, align 8
  %15 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %16 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %15, i64 0, i64 %14
  %17 = getelementptr inbounds %class.KV, %class.KV* %16, i32 0, i32 0
  %18 = bitcast %"union.KV<key, value, 0>::Key"* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %22

; <label>:21                                      ; preds = %2
  store %class.value* null, %class.value** %3, align 8
  br label %56

; <label>:22                                      ; preds = %2
  %23 = load i64, i64* %7, align 8
  %24 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %25 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %24, i64 0, i64 %23
  %26 = getelementptr inbounds %class.KV, %class.KV* %25, i32 0, i32 0
  %27 = bitcast %"union.KV<key, value, 0>::Key"* %26 to i64*
  %28 = load i64, i64* %27, align 8
  %29 = and i64 %28, 1
  %30 = icmp eq i64 %29, 0
  br i1 %30, label %31, label %48

; <label>:31                                      ; preds = %22
  %32 = load i64, i64* %7, align 8
  %33 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %34 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %33, i64 0, i64 %32
  %35 = getelementptr inbounds %class.KV, %class.KV* %34, i32 0, i32 0
  %36 = bitcast %"union.KV<key, value, 0>::Key"* %35 to %class.key**
  %37 = load %class.key*, %class.key** %36, align 8
  %38 = load %class.key*, %class.key** %5, align 8
  %39 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %37, %class.key* dereferenceable(8) %38)
  br i1 %39, label %40, label %47

; <label>:40                                      ; preds = %31
  %41 = load i64, i64* %7, align 8
  %42 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %43 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %42, i64 0, i64 %41
  %44 = getelementptr inbounds %class.KV, %class.KV* %43, i32 0, i32 1
  %45 = bitcast %"union.KV<key, value, 0>::Val"* %44 to %class.value**
  %46 = load %class.value*, %class.value** %45, align 8
  store %class.value* %46, %class.value** %3, align 8
  br label %56

; <label>:47                                      ; preds = %31
  store %class.value* null, %class.value** %3, align 8
  br label %56

; <label>:48                                      ; preds = %22
  %49 = load i64, i64* %7, align 8
  %50 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %51 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %50, i64 0, i64 %49
  %52 = load i64, i64* %6, align 8
  %53 = lshr i64 %52, 4
  %54 = load %class.key*, %class.key** %5, align 8
  %55 = call %class.value* @_ZN2KVI3key5valueLj0EE10inner_findERKS2_yPKS0_(%class.KV* dereferenceable(16) %51, i64 %53, %class.key* %54)
  store %class.value* %55, %class.value** %3, align 8
  br label %56

; <label>:56                                      ; preds = %48, %47, %40, %21
  %57 = load %class.value*, %class.value** %3, align 8
  ret %class.value* %57
}

; Function Attrs: ssp uwtable
define i64 @prim_hash_45set(i64, i64, i64) #0 {
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.hamt*, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca %class.value*, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  store i64 %2, i64* %6, align 8
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 6
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %3
  call void @fatal_err(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.32, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %3
  %15 = load i64, i64* %4, align 8
  %16 = and i64 %15, -8
  %17 = inttoptr i64 %16 to i64*
  %18 = bitcast i64* %17 to %class.hamt*
  store %class.hamt* %18, %class.hamt** %7, align 8
  %19 = call i8* @malloc(i64 8)
  %20 = bitcast i8* %19 to %class.key*
  %21 = bitcast %class.key* %20 to i8*
  %22 = bitcast i8* %21 to %class.key*
  %23 = load i64, i64* %5, align 8
  call void @_ZN3keyC1Ey(%class.key* %22, i64 %23)
  store %class.key* %22, %class.key** %8, align 8
  %24 = call i8* @malloc(i64 8)
  %25 = bitcast i8* %24 to %class.value*
  %26 = bitcast %class.value* %25 to i8*
  %27 = bitcast i8* %26 to %class.value*
  %28 = load i64, i64* %6, align 8
  call void @_ZN5valueC1Ey(%class.value* %27, i64 %28)
  store %class.value* %27, %class.value** %9, align 8
  %29 = load %class.hamt*, %class.hamt** %7, align 8
  %30 = load %class.key*, %class.key** %8, align 8
  %31 = load %class.value*, %class.value** %9, align 8
  %32 = call %class.hamt* @_ZNK4hamtI3key5valueE6insertEPKS0_PKS1_(%class.hamt* %29, %class.key* %30, %class.value* %31)
  %33 = ptrtoint %class.hamt* %32 to i64
  %34 = or i64 %33, 6
  ret i64 %34
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN5valueC1Ey(%class.value*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %class.value*, align 8
  %4 = alloca i64, align 8
  store %class.value* %0, %class.value** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %class.value*, %class.value** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN5valueC2Ey(%class.value* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI3key5valueE6insertEPKS0_PKS1_(%class.hamt*, %class.key*, %class.value*) #0 align 2 {
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %10 = load %class.hamt*, %class.hamt** %4, align 8
  %11 = load %class.key*, %class.key** %5, align 8
  %12 = call i64 @_ZNK3key4hashEv(%class.key* %11)
  store i64 %12, i64* %7, align 8
  %13 = load i64, i64* %7, align 8
  %14 = and i64 %13, 15
  %15 = urem i64 %14, 7
  store i64 %15, i64* %8, align 8
  %16 = call i8* @malloc(i64 120)
  %17 = bitcast i8* %16 to %class.hamt*
  store %class.hamt* %17, %class.hamt** %9, align 8
  %18 = load %class.hamt*, %class.hamt** %9, align 8
  %19 = bitcast %class.hamt* %18 to i8*
  %20 = bitcast %class.hamt* %10 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %19, i8* %20, i64 120, i32 8, i1 false)
  %21 = load i64, i64* %8, align 8
  %22 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %23 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %22, i64 0, i64 %21
  %24 = getelementptr inbounds %class.KV, %class.KV* %23, i32 0, i32 0
  %25 = bitcast %"union.KV<key, value, 0>::Key"* %24 to i64*
  %26 = load i64, i64* %25, align 8
  %27 = icmp eq i64 %26, 0
  br i1 %27, label %28, label %41

; <label>:28                                      ; preds = %3
  %29 = load i64, i64* %8, align 8
  %30 = load %class.hamt*, %class.hamt** %9, align 8
  %31 = getelementptr inbounds %class.hamt, %class.hamt* %30, i32 0, i32 0
  %32 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %31, i64 0, i64 %29
  %33 = bitcast %class.KV* %32 to i8*
  %34 = bitcast i8* %33 to %class.KV*
  %35 = load %class.key*, %class.key** %5, align 8
  %36 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EPKS0_PKS1_(%class.KV* %34, %class.key* %35, %class.value* %36)
  %37 = load %class.hamt*, %class.hamt** %9, align 8
  %38 = getelementptr inbounds %class.hamt, %class.hamt* %37, i32 0, i32 1
  %39 = load i64, i64* %38, align 8
  %40 = add i64 %39, 1
  store i64 %40, i64* %38, align 8
  br label %121

; <label>:41                                      ; preds = %3
  %42 = load i64, i64* %8, align 8
  %43 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %44 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %43, i64 0, i64 %42
  %45 = getelementptr inbounds %class.KV, %class.KV* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 0>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %104

; <label>:50                                      ; preds = %41
  %51 = load i64, i64* %8, align 8
  %52 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %53 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %52, i64 0, i64 %51
  %54 = getelementptr inbounds %class.KV, %class.KV* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 0>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %5, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %68

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %8, align 8
  %61 = load %class.hamt*, %class.hamt** %9, align 8
  %62 = getelementptr inbounds %class.hamt, %class.hamt* %61, i32 0, i32 0
  %63 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %62, i64 0, i64 %60
  %64 = bitcast %class.KV* %63 to i8*
  %65 = bitcast i8* %64 to %class.KV*
  %66 = load %class.key*, %class.key** %5, align 8
  %67 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EPKS0_PKS1_(%class.KV* %65, %class.key* %66, %class.value* %67)
  br label %103

; <label>:68                                      ; preds = %50
  %69 = load %class.hamt*, %class.hamt** %9, align 8
  %70 = getelementptr inbounds %class.hamt, %class.hamt* %69, i32 0, i32 1
  %71 = load i64, i64* %70, align 8
  %72 = add i64 %71, 1
  store i64 %72, i64* %70, align 8
  %73 = load i64, i64* %8, align 8
  %74 = load %class.hamt*, %class.hamt** %9, align 8
  %75 = getelementptr inbounds %class.hamt, %class.hamt* %74, i32 0, i32 0
  %76 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %75, i64 0, i64 %73
  %77 = bitcast %class.KV* %76 to i8*
  %78 = bitcast i8* %77 to %class.KV*
  %79 = load i64, i64* %8, align 8
  %80 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %81 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %80, i64 0, i64 %79
  %82 = getelementptr inbounds %class.KV, %class.KV* %81, i32 0, i32 0
  %83 = bitcast %"union.KV<key, value, 0>::Key"* %82 to %class.key**
  %84 = load %class.key*, %class.key** %83, align 8
  %85 = call i64 @_ZNK3key4hashEv(%class.key* %84)
  %86 = lshr i64 %85, 4
  %87 = load i64, i64* %8, align 8
  %88 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %89 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %88, i64 0, i64 %87
  %90 = getelementptr inbounds %class.KV, %class.KV* %89, i32 0, i32 0
  %91 = bitcast %"union.KV<key, value, 0>::Key"* %90 to %class.key**
  %92 = load %class.key*, %class.key** %91, align 8
  %93 = load i64, i64* %8, align 8
  %94 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %95 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %94, i64 0, i64 %93
  %96 = getelementptr inbounds %class.KV, %class.KV* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<key, value, 0>::Val"* %96 to %class.value**
  %98 = load %class.value*, %class.value** %97, align 8
  %99 = load i64, i64* %7, align 8
  %100 = lshr i64 %99, 4
  %101 = load %class.key*, %class.key** %5, align 8
  %102 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj0EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV* sret %78, i64 %86, %class.key* %92, %class.value* %98, i64 %100, %class.key* %101, %class.value* %102)
  br label %103

; <label>:103                                     ; preds = %68, %59
  br label %120

; <label>:104                                     ; preds = %41
  %105 = load i64, i64* %8, align 8
  %106 = load %class.hamt*, %class.hamt** %9, align 8
  %107 = getelementptr inbounds %class.hamt, %class.hamt* %106, i32 0, i32 0
  %108 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %107, i64 0, i64 %105
  %109 = bitcast %class.KV* %108 to i8*
  %110 = bitcast i8* %109 to %class.KV*
  %111 = load i64, i64* %8, align 8
  %112 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %113 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %112, i64 0, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 4
  %116 = load %class.key*, %class.key** %5, align 8
  %117 = load %class.value*, %class.value** %6, align 8
  %118 = load %class.hamt*, %class.hamt** %9, align 8
  %119 = getelementptr inbounds %class.hamt, %class.hamt* %118, i32 0, i32 1
  call void @_ZN2KVI3key5valueLj0EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV* sret %110, %class.KV* dereferenceable(16) %113, i64 %115, %class.key* %116, %class.value* %117, i64* %119)
  br label %120

; <label>:120                                     ; preds = %104, %103
  br label %121

; <label>:121                                     ; preds = %120, %28
  %122 = load %class.hamt*, %class.hamt** %9, align 8
  ret %class.hamt* %122
}

; Function Attrs: ssp uwtable
define i64 @prim_hash_45remove(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 6
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([45 x i8], [45 x i8]* @.str.33, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %2
  %12 = load i64, i64* %3, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  %15 = bitcast i64* %14 to %class.hamt*
  store %class.hamt* %15, %class.hamt** %5, align 8
  %16 = call i8* @malloc(i64 8)
  %17 = bitcast i8* %16 to %class.key*
  %18 = bitcast %class.key* %17 to i8*
  %19 = bitcast i8* %18 to %class.key*
  %20 = load i64, i64* %4, align 8
  call void @_ZN3keyC1Ey(%class.key* %19, i64 %20)
  store %class.key* %19, %class.key** %6, align 8
  %21 = load %class.hamt*, %class.hamt** %5, align 8
  %22 = load %class.key*, %class.key** %6, align 8
  %23 = call %class.hamt* @_ZNK4hamtI3key5valueE6removeEPKS0_(%class.hamt* %21, %class.key* %22)
  %24 = ptrtoint %class.hamt* %23 to i64
  %25 = or i64 %24, 6
  ret i64 %25
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI3key5valueE6removeEPKS0_(%class.hamt*, %class.key*) #0 align 2 {
  %3 = alloca %class.hamt*, align 8
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.hamt*, align 8
  %9 = alloca i64, align 8
  %10 = alloca %class.KV, align 8
  %11 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %12 = load %class.hamt*, %class.hamt** %4, align 8
  %13 = load %class.key*, %class.key** %5, align 8
  %14 = call i64 @_ZNK3key4hashEv(%class.key* %13)
  store i64 %14, i64* %6, align 8
  %15 = load i64, i64* %6, align 8
  %16 = and i64 %15, 15
  %17 = urem i64 %16, 7
  store i64 %17, i64* %7, align 8
  %18 = load i64, i64* %7, align 8
  %19 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %20 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %19, i64 0, i64 %18
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 0>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = icmp eq i64 %23, 0
  br i1 %24, label %25, label %26

; <label>:25                                      ; preds = %2
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:26                                      ; preds = %2
  %27 = load i64, i64* %7, align 8
  %28 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %29 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %28, i64 0, i64 %27
  %30 = getelementptr inbounds %class.KV, %class.KV* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 0>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = and i64 %32, 1
  %34 = icmp eq i64 %33, 0
  br i1 %34, label %35, label %61

; <label>:35                                      ; preds = %26
  %36 = load i64, i64* %7, align 8
  %37 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %38 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %37, i64 0, i64 %36
  %39 = getelementptr inbounds %class.KV, %class.KV* %38, i32 0, i32 0
  %40 = bitcast %"union.KV<key, value, 0>::Key"* %39 to %class.key**
  %41 = load %class.key*, %class.key** %40, align 8
  %42 = load %class.key*, %class.key** %5, align 8
  %43 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %41, %class.key* dereferenceable(8) %42)
  br i1 %43, label %44, label %60

; <label>:44                                      ; preds = %35
  %45 = call i8* @malloc(i64 120)
  %46 = bitcast i8* %45 to %class.hamt*
  store %class.hamt* %46, %class.hamt** %8, align 8
  %47 = load %class.hamt*, %class.hamt** %8, align 8
  %48 = bitcast %class.hamt* %47 to i8*
  %49 = bitcast %class.hamt* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %48, i8* %49, i64 120, i32 8, i1 false)
  %50 = load i64, i64* %7, align 8
  %51 = load %class.hamt*, %class.hamt** %8, align 8
  %52 = getelementptr inbounds %class.hamt, %class.hamt* %51, i64 %50
  %53 = bitcast %class.hamt* %52 to i8*
  %54 = bitcast i8* %53 to %class.KV*
  call void @_ZN2KVI3key5valueLj0EEC1EPKS0_PKS1_(%class.KV* %54, %class.key* null, %class.value* null)
  %55 = load %class.hamt*, %class.hamt** %8, align 8
  %56 = getelementptr inbounds %class.hamt, %class.hamt* %55, i32 0, i32 1
  %57 = load i64, i64* %56, align 8
  %58 = add i64 %57, -1
  store i64 %58, i64* %56, align 8
  %59 = load %class.hamt*, %class.hamt** %8, align 8
  store %class.hamt* %59, %class.hamt** %3, align 8
  br label %91

; <label>:60                                      ; preds = %35
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:61                                      ; preds = %26
  %62 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 1
  %63 = load i64, i64* %62, align 8
  store i64 %63, i64* %9, align 8
  %64 = load i64, i64* %7, align 8
  %65 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %66 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %65, i64 0, i64 %64
  %67 = load i64, i64* %6, align 8
  %68 = lshr i64 %67, 4
  %69 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj0EE12remove_innerERKS2_yPKS0_Py(%class.KV* sret %10, %class.KV* dereferenceable(16) %66, i64 %68, %class.key* %69, i64* %9)
  %70 = load i64, i64* %7, align 8
  %71 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %72 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %71, i64 0, i64 %70
  %73 = call zeroext i1 @_ZNK2KVI3key5valueLj0EEeqERKS2_(%class.KV* %10, %class.KV* dereferenceable(16) %72)
  br i1 %73, label %74, label %75

; <label>:74                                      ; preds = %61
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:75                                      ; preds = %61
  %76 = call i8* @malloc(i64 120)
  %77 = bitcast i8* %76 to %class.hamt*
  store %class.hamt* %77, %class.hamt** %11, align 8
  %78 = load %class.hamt*, %class.hamt** %11, align 8
  %79 = bitcast %class.hamt* %78 to i8*
  %80 = bitcast %class.hamt* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %79, i8* %80, i64 120, i32 8, i1 false)
  %81 = load i64, i64* %7, align 8
  %82 = load %class.hamt*, %class.hamt** %11, align 8
  %83 = getelementptr inbounds %class.hamt, %class.hamt* %82, i32 0, i32 0
  %84 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %83, i64 0, i64 %81
  %85 = bitcast %class.KV* %84 to i8*
  %86 = bitcast i8* %85 to %class.KV*
  call void @_ZN2KVI3key5valueLj0EEC1ERKS2_(%class.KV* %86, %class.KV* dereferenceable(16) %10)
  %87 = load i64, i64* %9, align 8
  %88 = load %class.hamt*, %class.hamt** %11, align 8
  %89 = getelementptr inbounds %class.hamt, %class.hamt* %88, i32 0, i32 1
  store i64 %87, i64* %89, align 8
  %90 = load %class.hamt*, %class.hamt** %11, align 8
  store %class.hamt* %90, %class.hamt** %3, align 8
  br label %91

; <label>:91                                      ; preds = %75, %74, %60, %44, %25
  %92 = load %class.hamt*, %class.hamt** %3, align 8
  ret %class.hamt* %92
}

; Function Attrs: ssp uwtable
define i64 @prim_hash_45_has45_key64(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.hamt*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.value*, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %9 = load i64, i64* %4, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 6
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.34, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %2
  %14 = load i64, i64* %4, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  %17 = bitcast i64* %16 to %class.hamt*
  store %class.hamt* %17, %class.hamt** %6, align 8
  %18 = call i8* @malloc(i64 8)
  %19 = bitcast i8* %18 to %class.key*
  %20 = bitcast %class.key* %19 to i8*
  %21 = bitcast i8* %20 to %class.key*
  %22 = load i64, i64* %5, align 8
  call void @_ZN3keyC1Ey(%class.key* %21, i64 %22)
  store %class.key* %21, %class.key** %7, align 8
  %23 = load %class.hamt*, %class.hamt** %6, align 8
  %24 = load %class.key*, %class.key** %7, align 8
  %25 = call %class.value* @_ZNK4hamtI3key5valueE3getEPKS0_(%class.hamt* %23, %class.key* %24)
  store %class.value* %25, %class.value** %8, align 8
  %26 = load %class.value*, %class.value** %8, align 8
  %27 = icmp eq %class.value* %26, null
  br i1 %27, label %28, label %29

; <label>:28                                      ; preds = %13
  store i64 15, i64* %3, align 8
  br label %30

; <label>:29                                      ; preds = %13
  store i64 31, i64* %3, align 8
  br label %30

; <label>:30                                      ; preds = %29, %28
  %31 = load i64, i64* %3, align 8
  ret i64 %31
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_eq_63(i64, i64) #5 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = load i64, i64* %5, align 8
  %8 = icmp eq i64 %6, %7
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  store i64 31, i64* %3, align 8
  br label %11

; <label>:10                                      ; preds = %2
  store i64 15, i64* %3, align 8
  br label %11

; <label>:11                                      ; preds = %10, %9
  %12 = load i64, i64* %3, align 8
  ret i64 %12
}

; Function Attrs: ssp uwtable
define i64 @applyprim_eq_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_eq_63(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_eqv_63(i64, i64) #5 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = load i64, i64* %5, align 8
  %8 = icmp eq i64 %6, %7
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  store i64 31, i64* %3, align 8
  br label %11

; <label>:10                                      ; preds = %2
  store i64 15, i64* %3, align 8
  br label %11

; <label>:11                                      ; preds = %10, %9
  %12 = load i64, i64* %3, align 8
  ret i64 %12
}

; Function Attrs: ssp uwtable
define i64 @applyprim_eqv_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_eqv_63(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_number_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: ssp uwtable
define i64 @applyprim_number_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_number_63(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_integer_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: ssp uwtable
define i64 @applyprim_integer_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_integer_63(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_void_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 39
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: ssp uwtable
define i64 @applyprim_void(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_void_63(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_procedure_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: ssp uwtable
define i64 @applyprim_procedure_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_procedure_63(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_null_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: ssp uwtable
define i64 @applyprim_null_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_null_63(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_cons_63(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 1
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: ssp uwtable
define i64 @applyprim_cons_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_cons_63(i64 %6)
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @prim_cons(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = call i64* @alloc(i64 16)
  store i64* %6, i64** %5, align 8
  %7 = load i64, i64* %3, align 8
  %8 = load i64*, i64** %5, align 8
  %9 = getelementptr inbounds i64, i64* %8, i64 0
  store i64 %7, i64* %9, align 8
  %10 = load i64, i64* %4, align 8
  %11 = load i64*, i64** %5, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  store i64 %10, i64* %12, align 8
  %13 = load i64*, i64** %5, align 8
  %14 = ptrtoint i64* %13 to i64
  %15 = or i64 %14, 1
  ret i64 %15
}

; Function Attrs: ssp uwtable
define i64 @applyprim_cons(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.25, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_cons(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: ssp uwtable
define i64 @prim_car(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %5 = load i64, i64* %2, align 8
  %6 = call i64 @expect_cons(i64 %5, i64* %3)
  store i64 %6, i64* %4, align 8
  %7 = load i64, i64* %4, align 8
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @applyprim_car(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_car(i64 %6)
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @prim_cdr(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %5 = load i64, i64* %2, align 8
  %6 = call i64 @expect_cons(i64 %5, i64* %3)
  store i64 %6, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @applyprim_cdr(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_cdr(i64 %6)
  ret i64 %7
}

; Function Attrs: ssp uwtable
define i64 @prim__43(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.35, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = load i64, i64* %4, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = add nsw i64 %17, %20
  %22 = shl i64 %21, 32
  %23 = or i64 %22, 2
  ret i64 %23
}

; Function Attrs: ssp uwtable
define i64 @applyprim__43(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 2, i64* %2, align 8
  br label %31

; <label>:8                                       ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = load i64*, i64** %4, align 8
  %23 = getelementptr inbounds i64, i64* %22, i64 1
  %24 = load i64, i64* %23, align 8
  %25 = call i64 @applyprim__43(i64 %24)
  %26 = and i64 %25, -8
  %27 = lshr i64 %26, 32
  %28 = add nsw i64 %21, %27
  %29 = shl i64 %28, 32
  %30 = or i64 %29, 2
  store i64 %30, i64* %2, align 8
  br label %31

; <label>:31                                      ; preds = %13, %7
  %32 = load i64, i64* %2, align 8
  ret i64 %32
}

; Function Attrs: ssp uwtable
define i64 @prim__45(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.35, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.38, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = load i64, i64* %4, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = sub nsw i64 %17, %20
  %22 = shl i64 %21, 32
  %23 = or i64 %22, 2
  ret i64 %23
}

; Function Attrs: ssp uwtable
define i64 @applyprim__45(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 2, i64* %2, align 8
  br label %45

; <label>:8                                       ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 1
  %19 = load i64, i64* %18, align 8
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %30

; <label>:21                                      ; preds = %13
  %22 = load i64*, i64** %4, align 8
  %23 = getelementptr inbounds i64, i64* %22, i64 0
  %24 = load i64, i64* %23, align 8
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = sub nsw i64 0, %26
  %28 = shl i64 %27, 32
  %29 = or i64 %28, 2
  store i64 %29, i64* %2, align 8
  br label %45

; <label>:30                                      ; preds = %13
  %31 = load i64*, i64** %4, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 0
  %33 = load i64, i64* %32, align 8
  %34 = and i64 %33, -8
  %35 = lshr i64 %34, 32
  %36 = load i64*, i64** %4, align 8
  %37 = getelementptr inbounds i64, i64* %36, i64 1
  %38 = load i64, i64* %37, align 8
  %39 = call i64 @applyprim__43(i64 %38)
  %40 = and i64 %39, -8
  %41 = lshr i64 %40, 32
  %42 = sub nsw i64 %35, %41
  %43 = shl i64 %42, 32
  %44 = or i64 %43, 2
  store i64 %44, i64* %2, align 8
  br label %45

; <label>:45                                      ; preds = %30, %21, %7
  %46 = load i64, i64* %2, align 8
  ret i64 %46
}

; Function Attrs: ssp uwtable
define i64 @prim__42(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.39, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.40, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = load i64, i64* %4, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = mul nsw i64 %17, %20
  %22 = shl i64 %21, 32
  %23 = or i64 %22, 2
  ret i64 %23
}

; Function Attrs: ssp uwtable
define i64 @applyprim__42(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %1
  store i64 4294967298, i64* %2, align 8
  br label %31

; <label>:8                                       ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = load i64*, i64** %4, align 8
  %23 = getelementptr inbounds i64, i64* %22, i64 1
  %24 = load i64, i64* %23, align 8
  %25 = call i64 @applyprim__42(i64 %24)
  %26 = and i64 %25, -8
  %27 = lshr i64 %26, 32
  %28 = mul nsw i64 %21, %27
  %29 = shl i64 %28, 32
  %30 = or i64 %29, 2
  store i64 %30, i64* %2, align 8
  br label %31

; <label>:31                                      ; preds = %13, %7
  %32 = load i64, i64* %2, align 8
  ret i64 %32
}

; Function Attrs: ssp uwtable
define i64 @prim__47(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.41, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13                                      ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.42, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = load i64, i64* %4, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = sdiv i64 %17, %20
  %22 = shl i64 %21, 32
  %23 = or i64 %22, 2
  ret i64 %23
}

; Function Attrs: ssp uwtable
define i64 @prim__61(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.43, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.44, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = load i64, i64* %5, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = icmp eq i64 %18, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %15
  store i64 31, i64* %3, align 8
  br label %25

; <label>:24                                      ; preds = %15
  store i64 15, i64* %3, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %3, align 8
  ret i64 %26
}

; Function Attrs: ssp uwtable
define i64 @prim__60(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.45, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.46, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = load i64, i64* %5, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = icmp slt i64 %18, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %15
  store i64 31, i64* %3, align 8
  br label %25

; <label>:24                                      ; preds = %15
  store i64 15, i64* %3, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %3, align 8
  ret i64 %26
}

; Function Attrs: ssp uwtable
define i64 @prim__60_61(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9                                       ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.47, i32 0, i32 0))
  br label %10

; <label>:10                                      ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.48, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = load i64, i64* %5, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = icmp sle i64 %18, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %15
  store i64 31, i64* %3, align 8
  br label %25

; <label>:24                                      ; preds = %15
  store i64 15, i64* %3, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %3, align 8
  ret i64 %26
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_not(i64) #5 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 15
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7                                       ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: ssp uwtable
define i64 @applyprim_not(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_not(i64 %6)
  ret i64 %7
}

; Function Attrs: nounwind ssp uwtable
define i64 @prim_and(i64, i64) #5 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = icmp eq i64 %6, 15
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %2
  store i64 15, i64* %3, align 8
  br label %14

; <label>:9                                       ; preds = %2
  %10 = load i64, i64* %5, align 8
  %11 = icmp eq i64 %10, 15
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %9
  store i64 15, i64* %3, align 8
  br label %14

; <label>:13                                      ; preds = %9
  store i64 31, i64* %3, align 8
  br label %14

; <label>:14                                      ; preds = %13, %12, %8
  %15 = load i64, i64* %3, align 8
  ret i64 %15
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN3keyC2Ey(%class.key*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca i64, align 8
  store %class.key* %0, %class.key** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %class.key*, %class.key** %3, align 8
  %6 = getelementptr inbounds %class.key, %class.key* %5, i32 0, i32 0
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN5valueC2Ey(%class.value*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %class.value*, align 8
  %4 = alloca i64, align 8
  store %class.value* %0, %class.value** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %class.value*, %class.value** %3, align 8
  %6 = getelementptr inbounds %class.value, %class.value* %5, i32 0, i32 0
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN4hamtI3key5valueEC2Ev(%class.hamt*) unnamed_addr #0 align 2 {
  %2 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %2, align 8
  %3 = load %class.hamt*, %class.hamt** %2, align 8
  %4 = getelementptr inbounds %class.hamt, %class.hamt* %3, i32 0, i32 0
  %5 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %4, i32 0, i32 0
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i64 7
  br label %7

; <label>:7                                       ; preds = %7, %1
  %8 = phi %class.KV* [ %5, %1 ], [ %9, %7 ]
  call void @_ZN2KVI3key5valueLj0EEC1Ev(%class.KV* %8)
  %9 = getelementptr inbounds %class.KV, %class.KV* %8, i64 1
  %10 = icmp eq %class.KV* %9, %6
  br i1 %10, label %11, label %7

; <label>:11                                      ; preds = %7
  %12 = getelementptr inbounds %class.hamt, %class.hamt* %3, i32 0, i32 1
  store i64 0, i64* %12, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC1Ev(%class.KV*) unnamed_addr #0 align 2 {
  %2 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %2, align 8
  %3 = load %class.KV*, %class.KV** %2, align 8
  call void @_ZN2KVI3key5valueLj0EEC2Ev(%class.KV* %3)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC2Ev(%class.KV*) unnamed_addr #0 align 2 {
  %2 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %2, align 8
  %3 = load %class.KV*, %class.KV** %2, align 8
  %4 = getelementptr inbounds %class.KV, %class.KV* %3, i32 0, i32 0
  call void @_ZN2KVI3key5valueLj0EE3KeyC1Ey(%"union.KV<key, value, 0>::Key"* %4, i64 0)
  %5 = getelementptr inbounds %class.KV, %class.KV* %3, i32 0, i32 1
  call void @_ZN2KVI3key5valueLj0EE3ValC1EPKS1_(%"union.KV<key, value, 0>::Val"* %5, %class.value* null)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3KeyC1Ey(%"union.KV<key, value, 0>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 0>::Key"* %0, %"union.KV<key, value, 0>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 0>::Key"*, %"union.KV<key, value, 0>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj0EE3KeyC2Ey(%"union.KV<key, value, 0>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3ValC1EPKS1_(%"union.KV<key, value, 0>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 0>::Val"* %0, %"union.KV<key, value, 0>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Val"*, %"union.KV<key, value, 0>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj0EE3ValC2EPKS1_(%"union.KV<key, value, 0>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3KeyC2Ey(%"union.KV<key, value, 0>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 0>::Key"* %0, %"union.KV<key, value, 0>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 0>::Key"*, %"union.KV<key, value, 0>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 0>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3ValC2EPKS1_(%"union.KV<key, value, 0>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 0>::Val"* %0, %"union.KV<key, value, 0>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Val"*, %"union.KV<key, value, 0>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 0>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr i64 @_ZNK3key4hashEv(%class.key*) #5 align 2 {
  %2 = alloca %class.key*, align 8
  %3 = alloca i8*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store %class.key* %0, %class.key** %2, align 8
  %6 = load %class.key*, %class.key** %2, align 8
  %7 = bitcast %class.key* %6 to i8*
  store i8* %7, i8** %3, align 8
  store i64 -3750763034362895579, i64* %4, align 8
  store i64 0, i64* %5, align 8
  br label %8

; <label>:8                                       ; preds = %27, %1
  %9 = load i64, i64* %5, align 8
  %10 = icmp ult i64 %9, 8
  br i1 %10, label %11, label %29

; <label>:11                                      ; preds = %8
  %12 = load i64, i64* %4, align 8
  %13 = load i8*, i8** %3, align 8
  %14 = load i8, i8* %13, align 1
  %15 = zext i8 %14 to i64
  %16 = xor i64 %12, %15
  store i64 %16, i64* %4, align 8
  %17 = load i64, i64* %4, align 8
  %18 = mul i64 %17, 1099511628211
  store i64 %18, i64* %4, align 8
  br label %19

; <label>:19                                      ; preds = %11
  %20 = load i64, i64* %5, align 8
  %21 = add i64 %20, 1
  store i64 %21, i64* %5, align 8
  %22 = icmp ne i64 %21, 0
  br i1 %22, label %23, label %27

; <label>:23                                      ; preds = %19
  %24 = load i8*, i8** %3, align 8
  %25 = getelementptr inbounds i8, i8* %24, i32 1
  store i8* %25, i8** %3, align 8
  %26 = icmp ne i8* %25, null
  br label %27

; <label>:27                                      ; preds = %23, %19
  %28 = phi i1 [ false, %19 ], [ %26, %23 ]
  br label %8

; <label>:29                                      ; preds = %8
  %30 = load i64, i64* %4, align 8
  ret i64 %30
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK3keyeqERKS_(%class.key*, %class.key* dereferenceable(8)) #5 align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca %class.key*, align 8
  store %class.key* %0, %class.key** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %class.key*, %class.key** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  %7 = getelementptr inbounds %class.key, %class.key* %6, i32 0, i32 0
  %8 = load i64, i64* %7, align 8
  %9 = getelementptr inbounds %class.key, %class.key* %5, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp eq i64 %8, %10
  ret i1 %11
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj0EE10inner_findERKS2_yPKS0_(%class.KV* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.0*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV* %0, %class.KV** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV*, %class.KV** %5, align 8
  %17 = getelementptr inbounds %class.KV, %class.KV* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 0>::Val"* %17 to %class.KV.0**
  %19 = load %class.KV.0*, %class.KV.0** %18, align 8
  store %class.KV.0* %19, %class.KV.0** %9, align 8
  %20 = load %class.KV*, %class.KV** %5, align 8
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 0>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.0*, %class.KV.0** %9, align 8
  %44 = getelementptr inbounds %class.KV.0, %class.KV.0* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.0, %class.KV.0* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 1>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.0*, %class.KV.0** %9, align 8
  %53 = getelementptr inbounds %class.KV.0, %class.KV.0* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.0, %class.KV.0* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 1>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.0*, %class.KV.0** %9, align 8
  %62 = getelementptr inbounds %class.KV.0, %class.KV.0* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.0, %class.KV.0* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 1>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.0*, %class.KV.0** %9, align 8
  %70 = getelementptr inbounds %class.KV.0, %class.KV.0* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj1EE10inner_findERKS2_yPKS0_(%class.KV.0* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: nounwind readnone
declare i64 @llvm.ctpop.i64(i64) #1

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj1EE10inner_findERKS2_yPKS0_(%class.KV.0* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.0*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.1*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.0* %0, %class.KV.0** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.0*, %class.KV.0** %5, align 8
  %17 = getelementptr inbounds %class.KV.0, %class.KV.0* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 1>::Val"* %17 to %class.KV.1**
  %19 = load %class.KV.1*, %class.KV.1** %18, align 8
  store %class.KV.1* %19, %class.KV.1** %9, align 8
  %20 = load %class.KV.0*, %class.KV.0** %5, align 8
  %21 = getelementptr inbounds %class.KV.0, %class.KV.0* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 1>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.1*, %class.KV.1** %9, align 8
  %44 = getelementptr inbounds %class.KV.1, %class.KV.1* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.1, %class.KV.1* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 2>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.1*, %class.KV.1** %9, align 8
  %53 = getelementptr inbounds %class.KV.1, %class.KV.1* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.1, %class.KV.1* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 2>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.1*, %class.KV.1** %9, align 8
  %62 = getelementptr inbounds %class.KV.1, %class.KV.1* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.1, %class.KV.1* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 2>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.1*, %class.KV.1** %9, align 8
  %70 = getelementptr inbounds %class.KV.1, %class.KV.1* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj2EE10inner_findERKS2_yPKS0_(%class.KV.1* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj2EE10inner_findERKS2_yPKS0_(%class.KV.1* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.1*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.2*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.1* %0, %class.KV.1** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.1*, %class.KV.1** %5, align 8
  %17 = getelementptr inbounds %class.KV.1, %class.KV.1* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 2>::Val"* %17 to %class.KV.2**
  %19 = load %class.KV.2*, %class.KV.2** %18, align 8
  store %class.KV.2* %19, %class.KV.2** %9, align 8
  %20 = load %class.KV.1*, %class.KV.1** %5, align 8
  %21 = getelementptr inbounds %class.KV.1, %class.KV.1* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 2>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.2*, %class.KV.2** %9, align 8
  %44 = getelementptr inbounds %class.KV.2, %class.KV.2* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.2, %class.KV.2* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 3>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.2*, %class.KV.2** %9, align 8
  %53 = getelementptr inbounds %class.KV.2, %class.KV.2* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.2, %class.KV.2* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 3>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.2*, %class.KV.2** %9, align 8
  %62 = getelementptr inbounds %class.KV.2, %class.KV.2* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.2, %class.KV.2* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 3>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.2*, %class.KV.2** %9, align 8
  %70 = getelementptr inbounds %class.KV.2, %class.KV.2* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj3EE10inner_findERKS2_yPKS0_(%class.KV.2* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj3EE10inner_findERKS2_yPKS0_(%class.KV.2* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.2*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.3*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.2* %0, %class.KV.2** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.2*, %class.KV.2** %5, align 8
  %17 = getelementptr inbounds %class.KV.2, %class.KV.2* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 3>::Val"* %17 to %class.KV.3**
  %19 = load %class.KV.3*, %class.KV.3** %18, align 8
  store %class.KV.3* %19, %class.KV.3** %9, align 8
  %20 = load %class.KV.2*, %class.KV.2** %5, align 8
  %21 = getelementptr inbounds %class.KV.2, %class.KV.2* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 3>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.3*, %class.KV.3** %9, align 8
  %44 = getelementptr inbounds %class.KV.3, %class.KV.3* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.3, %class.KV.3* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 4>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.3*, %class.KV.3** %9, align 8
  %53 = getelementptr inbounds %class.KV.3, %class.KV.3* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.3, %class.KV.3* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 4>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.3*, %class.KV.3** %9, align 8
  %62 = getelementptr inbounds %class.KV.3, %class.KV.3* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.3, %class.KV.3* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 4>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.3*, %class.KV.3** %9, align 8
  %70 = getelementptr inbounds %class.KV.3, %class.KV.3* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj4EE10inner_findERKS2_yPKS0_(%class.KV.3* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj4EE10inner_findERKS2_yPKS0_(%class.KV.3* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.3*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.4*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.3* %0, %class.KV.3** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.3*, %class.KV.3** %5, align 8
  %17 = getelementptr inbounds %class.KV.3, %class.KV.3* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 4>::Val"* %17 to %class.KV.4**
  %19 = load %class.KV.4*, %class.KV.4** %18, align 8
  store %class.KV.4* %19, %class.KV.4** %9, align 8
  %20 = load %class.KV.3*, %class.KV.3** %5, align 8
  %21 = getelementptr inbounds %class.KV.3, %class.KV.3* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 4>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.4*, %class.KV.4** %9, align 8
  %44 = getelementptr inbounds %class.KV.4, %class.KV.4* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.4, %class.KV.4* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 5>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.4*, %class.KV.4** %9, align 8
  %53 = getelementptr inbounds %class.KV.4, %class.KV.4* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.4, %class.KV.4* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 5>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.4*, %class.KV.4** %9, align 8
  %62 = getelementptr inbounds %class.KV.4, %class.KV.4* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.4, %class.KV.4* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 5>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.4*, %class.KV.4** %9, align 8
  %70 = getelementptr inbounds %class.KV.4, %class.KV.4* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj5EE10inner_findERKS2_yPKS0_(%class.KV.4* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj5EE10inner_findERKS2_yPKS0_(%class.KV.4* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.4*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.5*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.4* %0, %class.KV.4** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.4*, %class.KV.4** %5, align 8
  %17 = getelementptr inbounds %class.KV.4, %class.KV.4* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 5>::Val"* %17 to %class.KV.5**
  %19 = load %class.KV.5*, %class.KV.5** %18, align 8
  store %class.KV.5* %19, %class.KV.5** %9, align 8
  %20 = load %class.KV.4*, %class.KV.4** %5, align 8
  %21 = getelementptr inbounds %class.KV.4, %class.KV.4* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 5>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.5*, %class.KV.5** %9, align 8
  %44 = getelementptr inbounds %class.KV.5, %class.KV.5* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.5, %class.KV.5* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 6>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.5*, %class.KV.5** %9, align 8
  %53 = getelementptr inbounds %class.KV.5, %class.KV.5* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.5, %class.KV.5* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 6>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.5*, %class.KV.5** %9, align 8
  %62 = getelementptr inbounds %class.KV.5, %class.KV.5* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.5, %class.KV.5* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 6>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.5*, %class.KV.5** %9, align 8
  %70 = getelementptr inbounds %class.KV.5, %class.KV.5* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj6EE10inner_findERKS2_yPKS0_(%class.KV.5* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj6EE10inner_findERKS2_yPKS0_(%class.KV.5* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.5*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.6*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.5* %0, %class.KV.5** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.5*, %class.KV.5** %5, align 8
  %17 = getelementptr inbounds %class.KV.5, %class.KV.5* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 6>::Val"* %17 to %class.KV.6**
  %19 = load %class.KV.6*, %class.KV.6** %18, align 8
  store %class.KV.6* %19, %class.KV.6** %9, align 8
  %20 = load %class.KV.5*, %class.KV.5** %5, align 8
  %21 = getelementptr inbounds %class.KV.5, %class.KV.5* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 6>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.6*, %class.KV.6** %9, align 8
  %44 = getelementptr inbounds %class.KV.6, %class.KV.6* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.6, %class.KV.6* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 7>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.6*, %class.KV.6** %9, align 8
  %53 = getelementptr inbounds %class.KV.6, %class.KV.6* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.6, %class.KV.6* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 7>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.6*, %class.KV.6** %9, align 8
  %62 = getelementptr inbounds %class.KV.6, %class.KV.6* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.6, %class.KV.6* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 7>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.6*, %class.KV.6** %9, align 8
  %70 = getelementptr inbounds %class.KV.6, %class.KV.6* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj7EE10inner_findERKS2_yPKS0_(%class.KV.6* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj7EE10inner_findERKS2_yPKS0_(%class.KV.6* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.6*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.7*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.6* %0, %class.KV.6** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.6*, %class.KV.6** %5, align 8
  %17 = getelementptr inbounds %class.KV.6, %class.KV.6* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 7>::Val"* %17 to %class.KV.7**
  %19 = load %class.KV.7*, %class.KV.7** %18, align 8
  store %class.KV.7* %19, %class.KV.7** %9, align 8
  %20 = load %class.KV.6*, %class.KV.6** %5, align 8
  %21 = getelementptr inbounds %class.KV.6, %class.KV.6* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 7>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.7*, %class.KV.7** %9, align 8
  %44 = getelementptr inbounds %class.KV.7, %class.KV.7* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.7, %class.KV.7* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 8>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.7*, %class.KV.7** %9, align 8
  %53 = getelementptr inbounds %class.KV.7, %class.KV.7* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.7, %class.KV.7* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 8>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.7*, %class.KV.7** %9, align 8
  %62 = getelementptr inbounds %class.KV.7, %class.KV.7* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.7, %class.KV.7* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 8>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.7*, %class.KV.7** %9, align 8
  %70 = getelementptr inbounds %class.KV.7, %class.KV.7* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj8EE10inner_findERKS2_yPKS0_(%class.KV.7* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj8EE10inner_findERKS2_yPKS0_(%class.KV.7* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.7*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.8*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.7* %0, %class.KV.7** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.7*, %class.KV.7** %5, align 8
  %17 = getelementptr inbounds %class.KV.7, %class.KV.7* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 8>::Val"* %17 to %class.KV.8**
  %19 = load %class.KV.8*, %class.KV.8** %18, align 8
  store %class.KV.8* %19, %class.KV.8** %9, align 8
  %20 = load %class.KV.7*, %class.KV.7** %5, align 8
  %21 = getelementptr inbounds %class.KV.7, %class.KV.7* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 8>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.8*, %class.KV.8** %9, align 8
  %44 = getelementptr inbounds %class.KV.8, %class.KV.8* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.8, %class.KV.8* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 9>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.8*, %class.KV.8** %9, align 8
  %53 = getelementptr inbounds %class.KV.8, %class.KV.8* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.8, %class.KV.8* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 9>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.8*, %class.KV.8** %9, align 8
  %62 = getelementptr inbounds %class.KV.8, %class.KV.8* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.8, %class.KV.8* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 9>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.8*, %class.KV.8** %9, align 8
  %70 = getelementptr inbounds %class.KV.8, %class.KV.8* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj9EE10inner_findERKS2_yPKS0_(%class.KV.8* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj9EE10inner_findERKS2_yPKS0_(%class.KV.8* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.9*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i64, align 8
  store %class.KV.8* %0, %class.KV.8** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.8*, %class.KV.8** %5, align 8
  %17 = getelementptr inbounds %class.KV.8, %class.KV.8* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, value, 9>::Val"* %17 to %class.KV.9**
  %19 = load %class.KV.9*, %class.KV.9** %18, align 8
  store %class.KV.9* %19, %class.KV.9** %9, align 8
  %20 = load %class.KV.8*, %class.KV.8** %5, align 8
  %21 = getelementptr inbounds %class.KV.8, %class.KV.8* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, value, 9>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %75

; <label>:33                                      ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  %41 = sext i32 %40 to i64
  store i64 %41, i64* %12, align 8
  %42 = load i64, i64* %12, align 8
  %43 = load %class.KV.9*, %class.KV.9** %9, align 8
  %44 = getelementptr inbounds %class.KV.9, %class.KV.9* %43, i64 %42
  %45 = getelementptr inbounds %class.KV.9, %class.KV.9* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, value, 10>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %67

; <label>:50                                      ; preds = %33
  %51 = load i64, i64* %12, align 8
  %52 = load %class.KV.9*, %class.KV.9** %9, align 8
  %53 = getelementptr inbounds %class.KV.9, %class.KV.9* %52, i64 %51
  %54 = getelementptr inbounds %class.KV.9, %class.KV.9* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, value, 10>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %7, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %66

; <label>:59                                      ; preds = %50
  %60 = load i64, i64* %12, align 8
  %61 = load %class.KV.9*, %class.KV.9** %9, align 8
  %62 = getelementptr inbounds %class.KV.9, %class.KV.9* %61, i64 %60
  %63 = getelementptr inbounds %class.KV.9, %class.KV.9* %62, i32 0, i32 1
  %64 = bitcast %"union.KV<key, value, 10>::Val"* %63 to %class.value**
  %65 = load %class.value*, %class.value** %64, align 8
  store %class.value* %65, %class.value** %4, align 8
  br label %76

; <label>:66                                      ; preds = %50
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:67                                      ; preds = %33
  %68 = load i64, i64* %12, align 8
  %69 = load %class.KV.9*, %class.KV.9** %9, align 8
  %70 = getelementptr inbounds %class.KV.9, %class.KV.9* %69, i64 %68
  %71 = load i64, i64* %6, align 8
  %72 = lshr i64 %71, 6
  %73 = load %class.key*, %class.key** %7, align 8
  %74 = call %class.value* @_ZN2KVI3key5valueLj10EE10inner_findERKS2_yPKS0_(%class.KV.9* dereferenceable(16) %70, i64 %72, %class.key* %73)
  store %class.value* %74, %class.value** %4, align 8
  br label %76

; <label>:75                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %76

; <label>:76                                      ; preds = %75, %67, %66, %59
  %77 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %77
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZN2KVI3key5valueLj10EE10inner_findERKS2_yPKS0_(%class.KV.9* dereferenceable(16), i64, %class.key*) #0 align 2 {
  %4 = alloca %class.value*, align 8
  %5 = alloca %class.KV.9*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  store %class.KV.9* %0, %class.KV.9** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %8 = load %class.KV.9*, %class.KV.9** %5, align 8
  %9 = getelementptr inbounds %class.KV.9, %class.KV.9* %8, i32 0, i32 1
  %10 = bitcast %"union.KV<key, value, 10>::Val"* %9 to %class.LL**
  %11 = load %class.LL*, %class.LL** %10, align 8
  %12 = icmp ne %class.LL* %11, null
  br i1 %12, label %13, label %20

; <label>:13                                      ; preds = %3
  %14 = load %class.KV.9*, %class.KV.9** %5, align 8
  %15 = getelementptr inbounds %class.KV.9, %class.KV.9* %14, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 10>::Val"* %15 to %class.LL**
  %17 = load %class.LL*, %class.LL** %16, align 8
  %18 = load %class.key*, %class.key** %7, align 8
  %19 = call %class.value* @_ZNK2LLI3key5valueE4findEPKS0_(%class.LL* %17, %class.key* %18)
  store %class.value* %19, %class.value** %4, align 8
  br label %21

; <label>:20                                      ; preds = %3
  store %class.value* null, %class.value** %4, align 8
  br label %21

; <label>:21                                      ; preds = %20, %13
  %22 = load %class.value*, %class.value** %4, align 8
  ret %class.value* %22
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.value* @_ZNK2LLI3key5valueE4findEPKS0_(%class.LL*, %class.key*) #0 align 2 {
  %3 = alloca %class.value*, align 8
  %4 = alloca %class.LL*, align 8
  %5 = alloca %class.key*, align 8
  store %class.LL* %0, %class.LL** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %6 = load %class.LL*, %class.LL** %4, align 8
  %7 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 0
  %8 = load %class.key*, %class.key** %7, align 8
  %9 = load %class.key*, %class.key** %5, align 8
  %10 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %8, %class.key* dereferenceable(8) %9)
  br i1 %10, label %11, label %14

; <label>:11                                      ; preds = %2
  %12 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 1
  %13 = load %class.value*, %class.value** %12, align 8
  store %class.value* %13, %class.value** %3, align 8
  br label %24

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 2
  %16 = load %class.LL*, %class.LL** %15, align 8
  %17 = icmp ne %class.LL* %16, null
  br i1 %17, label %18, label %23

; <label>:18                                      ; preds = %14
  %19 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 2
  %20 = load %class.LL*, %class.LL** %19, align 8
  %21 = load %class.key*, %class.key** %5, align 8
  %22 = call %class.value* @_ZNK2LLI3key5valueE4findEPKS0_(%class.LL* %20, %class.key* %21)
  store %class.value* %22, %class.value** %3, align 8
  br label %24

; <label>:23                                      ; preds = %14
  store %class.value* null, %class.value** %3, align 8
  br label %24

; <label>:24                                      ; preds = %23, %18, %11
  %25 = load %class.value*, %class.value** %3, align 8
  ret %class.value* %25
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #7

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC1EPKS0_PKS1_(%class.KV*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC2EPKS0_PKS1_(%class.KV* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.0, align 8
  %17 = alloca %class.KV.0*, align 8
  %18 = alloca %class.KV.0*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj1EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.0* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.0*
  store %class.KV.0* %38, %class.KV.0** %17, align 8
  %39 = load %class.KV.0*, %class.KV.0** %17, align 8
  %40 = getelementptr inbounds %class.KV.0, %class.KV.0* %39, i64 0
  %41 = bitcast %class.KV.0* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.0*
  call void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0* %42, %class.KV.0* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.0*, %class.KV.0** %17, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %46, %class.KV.0* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.0*
  store %class.KV.0* %50, %class.KV.0** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.0*, %class.KV.0** %18, align 8
  %56 = getelementptr inbounds %class.KV.0, %class.KV.0* %55, i64 0
  %57 = bitcast %class.KV.0* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.0*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.0*, %class.KV.0** %18, align 8
  %62 = getelementptr inbounds %class.KV.0, %class.KV.0* %61, i64 1
  %63 = bitcast %class.KV.0* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.0*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.0*, %class.KV.0** %18, align 8
  %69 = getelementptr inbounds %class.KV.0, %class.KV.0* %68, i64 0
  %70 = bitcast %class.KV.0* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.0*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.0*, %class.KV.0** %18, align 8
  %75 = getelementptr inbounds %class.KV.0, %class.KV.0* %74, i64 1
  %76 = bitcast %class.KV.0* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.0*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.0*, %class.KV.0** %18, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %87, %class.KV.0* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV* noalias sret, %class.KV* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.0*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.0*, align 8
  %19 = alloca %class.KV.0, align 8
  %20 = alloca %class.KV.0, align 8
  %21 = alloca %class.KV.0*, align 8
  %22 = alloca %class.KV.0, align 8
  %23 = alloca %class.KV.0*, align 8
  %24 = alloca %class.KV.0*, align 8
  store %class.KV* %1, %class.KV** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV*, %class.KV** %7, align 8
  %26 = getelementptr inbounds %class.KV, %class.KV* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 0>::Val"* %26 to %class.KV.0**
  %28 = load %class.KV.0*, %class.KV.0** %27, align 8
  store %class.KV.0* %28, %class.KV.0** %12, align 8
  %29 = load %class.KV*, %class.KV** %7, align 8
  %30 = getelementptr inbounds %class.KV, %class.KV* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 0>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.0*, %class.KV.0** %12, align 8
  %60 = getelementptr inbounds %class.KV.0, %class.KV.0* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.0, %class.KV.0* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 1>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.0*, %class.KV.0** %12, align 8
  %69 = getelementptr inbounds %class.KV.0, %class.KV.0* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.0, %class.KV.0* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 1>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.0*, %class.KV.0** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.0* @_ZN2KVI3key5valueLj1EE11update_nodeEPKS2_mmRS3_(%class.KV.0* %76, i64 %77, i64 %78, %class.KV.0* dereferenceable(16) %19)
  store %class.KV.0* %81, %class.KV.0** %18, align 8
  %82 = load %class.KV*, %class.KV** %7, align 8
  %83 = getelementptr inbounds %class.KV, %class.KV* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 0>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.0*, %class.KV.0** %18, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %85, %class.KV.0* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.0*, %class.KV.0** %12, align 8
  %93 = getelementptr inbounds %class.KV.0, %class.KV.0* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.0, %class.KV.0* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 1>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 10
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.0*, %class.KV.0** %12, align 8
  %101 = getelementptr inbounds %class.KV.0, %class.KV.0* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.0, %class.KV.0* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 1>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.0*, %class.KV.0** %12, align 8
  %107 = getelementptr inbounds %class.KV.0, %class.KV.0* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.0, %class.KV.0* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 1>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj1EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.0* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.0*, %class.KV.0** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.0* @_ZN2KVI3key5valueLj1EE11update_nodeEPKS2_mmRS3_(%class.KV.0* %115, i64 %116, i64 %117, %class.KV.0* dereferenceable(16) %20)
  store %class.KV.0* %118, %class.KV.0** %21, align 8
  %119 = load %class.KV*, %class.KV** %7, align 8
  %120 = getelementptr inbounds %class.KV, %class.KV* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 0>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.0*, %class.KV.0** %21, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %122, %class.KV.0* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.0*, %class.KV.0** %12, align 8
  %127 = getelementptr inbounds %class.KV.0, %class.KV.0* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj1EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.0* sret %22, %class.KV.0* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.0*, %class.KV.0** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.0* @_ZN2KVI3key5valueLj1EE11update_nodeEPKS2_mmRS3_(%class.KV.0* %133, i64 %134, i64 %135, %class.KV.0* dereferenceable(16) %22)
  store %class.KV.0* %136, %class.KV.0** %23, align 8
  %137 = load %class.KV*, %class.KV** %7, align 8
  %138 = getelementptr inbounds %class.KV, %class.KV* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 0>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.0*, %class.KV.0** %23, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %140, %class.KV.0* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.0*
  store %class.KV.0* %150, %class.KV.0** %24, align 8
  %151 = load %class.KV.0*, %class.KV.0** %24, align 8
  %152 = bitcast %class.KV.0* %151 to i8*
  %153 = load %class.KV.0*, %class.KV.0** %12, align 8
  %154 = bitcast %class.KV.0* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.0*, %class.KV.0** %24, align 8
  %160 = getelementptr inbounds %class.KV.0, %class.KV.0* %159, i64 %158
  %161 = bitcast %class.KV.0* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.0*, %class.KV.0** %12, align 8
  %164 = getelementptr inbounds %class.KV.0, %class.KV.0* %163, i64 %162
  %165 = bitcast %class.KV.0* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.0*, %class.KV.0** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.0, %class.KV.0* %170, i64 %171
  %173 = bitcast %class.KV.0* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.0*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.0*, %class.KV.0** %24, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %182, %class.KV.0* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC2EPKS0_PKS1_(%class.KV*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj0EE3KeyC1EPKS0_(%"union.KV<key, value, 0>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj0EE3ValC1EPKS1_(%"union.KV<key, value, 0>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3KeyC1EPKS0_(%"union.KV<key, value, 0>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 0>::Key"* %0, %"union.KV<key, value, 0>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Key"*, %"union.KV<key, value, 0>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj0EE3KeyC2EPKS0_(%"union.KV<key, value, 0>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3KeyC2EPKS0_(%"union.KV<key, value, 0>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 0>::Key"* %0, %"union.KV<key, value, 0>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Key"*, %"union.KV<key, value, 0>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 0>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.0* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.1, align 8
  %17 = alloca %class.KV.1*, align 8
  %18 = alloca %class.KV.1*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj2EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.1* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.1*
  store %class.KV.1* %38, %class.KV.1** %17, align 8
  %39 = load %class.KV.1*, %class.KV.1** %17, align 8
  %40 = getelementptr inbounds %class.KV.1, %class.KV.1* %39, i64 0
  %41 = bitcast %class.KV.1* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.1*
  call void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1* %42, %class.KV.1* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.1*, %class.KV.1** %17, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %46, %class.KV.1* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.1*
  store %class.KV.1* %50, %class.KV.1** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.1*, %class.KV.1** %18, align 8
  %56 = getelementptr inbounds %class.KV.1, %class.KV.1* %55, i64 0
  %57 = bitcast %class.KV.1* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.1*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.1*, %class.KV.1** %18, align 8
  %62 = getelementptr inbounds %class.KV.1, %class.KV.1* %61, i64 1
  %63 = bitcast %class.KV.1* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.1*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.1*, %class.KV.1** %18, align 8
  %69 = getelementptr inbounds %class.KV.1, %class.KV.1* %68, i64 0
  %70 = bitcast %class.KV.1* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.1*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.1*, %class.KV.1** %18, align 8
  %75 = getelementptr inbounds %class.KV.1, %class.KV.1* %74, i64 1
  %76 = bitcast %class.KV.1* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.1*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.1*, %class.KV.1** %18, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %87, %class.KV.1* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0*, %class.KV.0* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.0*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %class.KV.0*, %class.KV.0** %3, align 8
  %6 = load %class.KV.0*, %class.KV.0** %4, align 8
  call void @_ZN2KVI3key5valueLj1EEC2ERKS2_(%class.KV.0* %5, %class.KV.0* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV*, i64, %class.KV.0*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.0*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.0* %2, %class.KV.0** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC2EyPKS_IS0_S1_Lj1EE(%class.KV* %7, i64 %8, %class.KV.0* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC1EPKS0_PKS1_(%class.KV.0*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj1EEC2EPKS0_PKS1_(%class.KV.0* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.1* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.2, align 8
  %17 = alloca %class.KV.2*, align 8
  %18 = alloca %class.KV.2*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj3EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.2* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.2*
  store %class.KV.2* %38, %class.KV.2** %17, align 8
  %39 = load %class.KV.2*, %class.KV.2** %17, align 8
  %40 = getelementptr inbounds %class.KV.2, %class.KV.2* %39, i64 0
  %41 = bitcast %class.KV.2* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.2*
  call void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2* %42, %class.KV.2* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.2*, %class.KV.2** %17, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %46, %class.KV.2* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.2*
  store %class.KV.2* %50, %class.KV.2** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.2*, %class.KV.2** %18, align 8
  %56 = getelementptr inbounds %class.KV.2, %class.KV.2* %55, i64 0
  %57 = bitcast %class.KV.2* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.2*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.2*, %class.KV.2** %18, align 8
  %62 = getelementptr inbounds %class.KV.2, %class.KV.2* %61, i64 1
  %63 = bitcast %class.KV.2* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.2*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.2*, %class.KV.2** %18, align 8
  %69 = getelementptr inbounds %class.KV.2, %class.KV.2* %68, i64 0
  %70 = bitcast %class.KV.2* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.2*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.2*, %class.KV.2** %18, align 8
  %75 = getelementptr inbounds %class.KV.2, %class.KV.2* %74, i64 1
  %76 = bitcast %class.KV.2* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.2*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.2*, %class.KV.2** %18, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %87, %class.KV.2* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1*, %class.KV.1* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.1*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %class.KV.1*, %class.KV.1** %3, align 8
  %6 = load %class.KV.1*, %class.KV.1** %4, align 8
  call void @_ZN2KVI3key5valueLj2EEC2ERKS2_(%class.KV.1* %5, %class.KV.1* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0*, i64, %class.KV.1*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.1*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.1* %2, %class.KV.1** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3key5valueLj1EEC2EyPKS_IS0_S1_Lj2EE(%class.KV.0* %7, i64 %8, %class.KV.1* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj2EEC2EPKS0_PKS1_(%class.KV.1* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.2* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.3, align 8
  %17 = alloca %class.KV.3*, align 8
  %18 = alloca %class.KV.3*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj4EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.3* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.3*
  store %class.KV.3* %38, %class.KV.3** %17, align 8
  %39 = load %class.KV.3*, %class.KV.3** %17, align 8
  %40 = getelementptr inbounds %class.KV.3, %class.KV.3* %39, i64 0
  %41 = bitcast %class.KV.3* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.3*
  call void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3* %42, %class.KV.3* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.3*, %class.KV.3** %17, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %46, %class.KV.3* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.3*
  store %class.KV.3* %50, %class.KV.3** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.3*, %class.KV.3** %18, align 8
  %56 = getelementptr inbounds %class.KV.3, %class.KV.3* %55, i64 0
  %57 = bitcast %class.KV.3* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.3*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.3*, %class.KV.3** %18, align 8
  %62 = getelementptr inbounds %class.KV.3, %class.KV.3* %61, i64 1
  %63 = bitcast %class.KV.3* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.3*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.3*, %class.KV.3** %18, align 8
  %69 = getelementptr inbounds %class.KV.3, %class.KV.3* %68, i64 0
  %70 = bitcast %class.KV.3* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.3*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.3*, %class.KV.3** %18, align 8
  %75 = getelementptr inbounds %class.KV.3, %class.KV.3* %74, i64 1
  %76 = bitcast %class.KV.3* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.3*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.3*, %class.KV.3** %18, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %87, %class.KV.3* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2*, %class.KV.2* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.2*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %class.KV.2*, %class.KV.2** %3, align 8
  %6 = load %class.KV.2*, %class.KV.2** %4, align 8
  call void @_ZN2KVI3key5valueLj3EEC2ERKS2_(%class.KV.2* %5, %class.KV.2* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1*, i64, %class.KV.2*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.2*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.2* %2, %class.KV.2** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3key5valueLj2EEC2EyPKS_IS0_S1_Lj3EE(%class.KV.1* %7, i64 %8, %class.KV.2* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj3EEC2EPKS0_PKS1_(%class.KV.2* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.3* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.4, align 8
  %17 = alloca %class.KV.4*, align 8
  %18 = alloca %class.KV.4*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj5EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.4* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.4*
  store %class.KV.4* %38, %class.KV.4** %17, align 8
  %39 = load %class.KV.4*, %class.KV.4** %17, align 8
  %40 = getelementptr inbounds %class.KV.4, %class.KV.4* %39, i64 0
  %41 = bitcast %class.KV.4* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.4*
  call void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4* %42, %class.KV.4* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.4*, %class.KV.4** %17, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %46, %class.KV.4* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.4*
  store %class.KV.4* %50, %class.KV.4** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.4*, %class.KV.4** %18, align 8
  %56 = getelementptr inbounds %class.KV.4, %class.KV.4* %55, i64 0
  %57 = bitcast %class.KV.4* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.4*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.4*, %class.KV.4** %18, align 8
  %62 = getelementptr inbounds %class.KV.4, %class.KV.4* %61, i64 1
  %63 = bitcast %class.KV.4* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.4*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.4*, %class.KV.4** %18, align 8
  %69 = getelementptr inbounds %class.KV.4, %class.KV.4* %68, i64 0
  %70 = bitcast %class.KV.4* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.4*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.4*, %class.KV.4** %18, align 8
  %75 = getelementptr inbounds %class.KV.4, %class.KV.4* %74, i64 1
  %76 = bitcast %class.KV.4* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.4*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.4*, %class.KV.4** %18, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %87, %class.KV.4* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3*, %class.KV.3* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.3*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %class.KV.3*, %class.KV.3** %3, align 8
  %6 = load %class.KV.3*, %class.KV.3** %4, align 8
  call void @_ZN2KVI3key5valueLj4EEC2ERKS2_(%class.KV.3* %5, %class.KV.3* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2*, i64, %class.KV.3*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.3*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.3* %2, %class.KV.3** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3key5valueLj3EEC2EyPKS_IS0_S1_Lj4EE(%class.KV.2* %7, i64 %8, %class.KV.3* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj4EEC2EPKS0_PKS1_(%class.KV.3* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.4* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.5, align 8
  %17 = alloca %class.KV.5*, align 8
  %18 = alloca %class.KV.5*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj6EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.5* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.5*
  store %class.KV.5* %38, %class.KV.5** %17, align 8
  %39 = load %class.KV.5*, %class.KV.5** %17, align 8
  %40 = getelementptr inbounds %class.KV.5, %class.KV.5* %39, i64 0
  %41 = bitcast %class.KV.5* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.5*
  call void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5* %42, %class.KV.5* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.5*, %class.KV.5** %17, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %46, %class.KV.5* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.5*
  store %class.KV.5* %50, %class.KV.5** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.5*, %class.KV.5** %18, align 8
  %56 = getelementptr inbounds %class.KV.5, %class.KV.5* %55, i64 0
  %57 = bitcast %class.KV.5* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.5*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.5*, %class.KV.5** %18, align 8
  %62 = getelementptr inbounds %class.KV.5, %class.KV.5* %61, i64 1
  %63 = bitcast %class.KV.5* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.5*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.5*, %class.KV.5** %18, align 8
  %69 = getelementptr inbounds %class.KV.5, %class.KV.5* %68, i64 0
  %70 = bitcast %class.KV.5* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.5*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.5*, %class.KV.5** %18, align 8
  %75 = getelementptr inbounds %class.KV.5, %class.KV.5* %74, i64 1
  %76 = bitcast %class.KV.5* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.5*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.5*, %class.KV.5** %18, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %87, %class.KV.5* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4*, %class.KV.4* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.4*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %class.KV.4*, %class.KV.4** %3, align 8
  %6 = load %class.KV.4*, %class.KV.4** %4, align 8
  call void @_ZN2KVI3key5valueLj5EEC2ERKS2_(%class.KV.4* %5, %class.KV.4* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3*, i64, %class.KV.4*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.4*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.4* %2, %class.KV.4** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3key5valueLj4EEC2EyPKS_IS0_S1_Lj5EE(%class.KV.3* %7, i64 %8, %class.KV.4* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj5EEC2EPKS0_PKS1_(%class.KV.4* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.5* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.6, align 8
  %17 = alloca %class.KV.6*, align 8
  %18 = alloca %class.KV.6*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj7EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.6* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.6*
  store %class.KV.6* %38, %class.KV.6** %17, align 8
  %39 = load %class.KV.6*, %class.KV.6** %17, align 8
  %40 = getelementptr inbounds %class.KV.6, %class.KV.6* %39, i64 0
  %41 = bitcast %class.KV.6* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.6*
  call void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6* %42, %class.KV.6* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.6*, %class.KV.6** %17, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %46, %class.KV.6* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.6*
  store %class.KV.6* %50, %class.KV.6** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.6*, %class.KV.6** %18, align 8
  %56 = getelementptr inbounds %class.KV.6, %class.KV.6* %55, i64 0
  %57 = bitcast %class.KV.6* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.6*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.6*, %class.KV.6** %18, align 8
  %62 = getelementptr inbounds %class.KV.6, %class.KV.6* %61, i64 1
  %63 = bitcast %class.KV.6* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.6*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.6*, %class.KV.6** %18, align 8
  %69 = getelementptr inbounds %class.KV.6, %class.KV.6* %68, i64 0
  %70 = bitcast %class.KV.6* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.6*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.6*, %class.KV.6** %18, align 8
  %75 = getelementptr inbounds %class.KV.6, %class.KV.6* %74, i64 1
  %76 = bitcast %class.KV.6* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.6*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.6*, %class.KV.6** %18, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %87, %class.KV.6* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5*, %class.KV.5* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.5*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %class.KV.5*, %class.KV.5** %3, align 8
  %6 = load %class.KV.5*, %class.KV.5** %4, align 8
  call void @_ZN2KVI3key5valueLj6EEC2ERKS2_(%class.KV.5* %5, %class.KV.5* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4*, i64, %class.KV.5*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.5*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.5* %2, %class.KV.5** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3key5valueLj5EEC2EyPKS_IS0_S1_Lj6EE(%class.KV.4* %7, i64 %8, %class.KV.5* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj6EEC2EPKS0_PKS1_(%class.KV.5* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.6* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.7, align 8
  %17 = alloca %class.KV.7*, align 8
  %18 = alloca %class.KV.7*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj8EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.7* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.7*
  store %class.KV.7* %38, %class.KV.7** %17, align 8
  %39 = load %class.KV.7*, %class.KV.7** %17, align 8
  %40 = getelementptr inbounds %class.KV.7, %class.KV.7* %39, i64 0
  %41 = bitcast %class.KV.7* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.7*
  call void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7* %42, %class.KV.7* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.7*, %class.KV.7** %17, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %46, %class.KV.7* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.7*
  store %class.KV.7* %50, %class.KV.7** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.7*, %class.KV.7** %18, align 8
  %56 = getelementptr inbounds %class.KV.7, %class.KV.7* %55, i64 0
  %57 = bitcast %class.KV.7* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.7*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.7*, %class.KV.7** %18, align 8
  %62 = getelementptr inbounds %class.KV.7, %class.KV.7* %61, i64 1
  %63 = bitcast %class.KV.7* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.7*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.7*, %class.KV.7** %18, align 8
  %69 = getelementptr inbounds %class.KV.7, %class.KV.7* %68, i64 0
  %70 = bitcast %class.KV.7* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.7*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.7*, %class.KV.7** %18, align 8
  %75 = getelementptr inbounds %class.KV.7, %class.KV.7* %74, i64 1
  %76 = bitcast %class.KV.7* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.7*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.7*, %class.KV.7** %18, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %87, %class.KV.7* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6*, %class.KV.6* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.6*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %class.KV.6*, %class.KV.6** %3, align 8
  %6 = load %class.KV.6*, %class.KV.6** %4, align 8
  call void @_ZN2KVI3key5valueLj7EEC2ERKS2_(%class.KV.6* %5, %class.KV.6* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5*, i64, %class.KV.6*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.6*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.6* %2, %class.KV.6** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3key5valueLj6EEC2EyPKS_IS0_S1_Lj7EE(%class.KV.5* %7, i64 %8, %class.KV.6* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj7EEC2EPKS0_PKS1_(%class.KV.6* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.7* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.8, align 8
  %17 = alloca %class.KV.8*, align 8
  %18 = alloca %class.KV.8*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj9EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.8* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.8*
  store %class.KV.8* %38, %class.KV.8** %17, align 8
  %39 = load %class.KV.8*, %class.KV.8** %17, align 8
  %40 = getelementptr inbounds %class.KV.8, %class.KV.8* %39, i64 0
  %41 = bitcast %class.KV.8* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.8*
  call void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8* %42, %class.KV.8* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.8*, %class.KV.8** %17, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %46, %class.KV.8* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.8*
  store %class.KV.8* %50, %class.KV.8** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.8*, %class.KV.8** %18, align 8
  %56 = getelementptr inbounds %class.KV.8, %class.KV.8* %55, i64 0
  %57 = bitcast %class.KV.8* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.8*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.8*, %class.KV.8** %18, align 8
  %62 = getelementptr inbounds %class.KV.8, %class.KV.8* %61, i64 1
  %63 = bitcast %class.KV.8* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.8*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.8*, %class.KV.8** %18, align 8
  %69 = getelementptr inbounds %class.KV.8, %class.KV.8* %68, i64 0
  %70 = bitcast %class.KV.8* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.8*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.8*, %class.KV.8** %18, align 8
  %75 = getelementptr inbounds %class.KV.8, %class.KV.8* %74, i64 1
  %76 = bitcast %class.KV.8* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.8*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.8*, %class.KV.8** %18, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %87, %class.KV.8* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7*, %class.KV.7* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.7*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %class.KV.7*, %class.KV.7** %3, align 8
  %6 = load %class.KV.7*, %class.KV.7** %4, align 8
  call void @_ZN2KVI3key5valueLj8EEC2ERKS2_(%class.KV.7* %5, %class.KV.7* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6*, i64, %class.KV.7*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.7*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.7* %2, %class.KV.7** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3key5valueLj7EEC2EyPKS_IS0_S1_Lj8EE(%class.KV.6* %7, i64 %8, %class.KV.7* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj8EEC2EPKS0_PKS1_(%class.KV.7* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.8* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.9, align 8
  %17 = alloca %class.KV.9*, align 8
  %18 = alloca %class.KV.9*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  store i64 %21, i64* %14, align 8
  %22 = load i64, i64* %11, align 8
  %23 = and i64 %22, 63
  %24 = urem i64 %23, 63
  store i64 %24, i64* %15, align 8
  %25 = load i64, i64* %14, align 8
  %26 = load i64, i64* %15, align 8
  %27 = icmp eq i64 %25, %26
  br i1 %27, label %28, label %48

; <label>:28                                      ; preds = %7
  %29 = load i64, i64* %8, align 8
  %30 = lshr i64 %29, 6
  %31 = load %class.key*, %class.key** %9, align 8
  %32 = load %class.value*, %class.value** %10, align 8
  %33 = load i64, i64* %11, align 8
  %34 = lshr i64 %33, 6
  %35 = load %class.key*, %class.key** %12, align 8
  %36 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj10EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.9* sret %16, i64 %30, %class.key* %31, %class.value* %32, i64 %34, %class.key* %35, %class.value* %36)
  %37 = call i8* @malloc(i64 16)
  %38 = bitcast i8* %37 to %class.KV.9*
  store %class.KV.9* %38, %class.KV.9** %17, align 8
  %39 = load %class.KV.9*, %class.KV.9** %17, align 8
  %40 = getelementptr inbounds %class.KV.9, %class.KV.9* %39, i64 0
  %41 = bitcast %class.KV.9* %40 to i8*
  %42 = bitcast i8* %41 to %class.KV.9*
  call void @_ZN2KVI3key5valueLj10EEC1ERKS2_(%class.KV.9* %42, %class.KV.9* dereferenceable(16) %16)
  %43 = load i64, i64* %14, align 8
  %44 = shl i64 1, %43
  %45 = shl i64 %44, 1
  %46 = or i64 %45, 1
  %47 = load %class.KV.9*, %class.KV.9** %17, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %46, %class.KV.9* %47)
  br label %89

; <label>:48                                      ; preds = %7
  %49 = call i8* @malloc(i64 32)
  %50 = bitcast i8* %49 to %class.KV.9*
  store %class.KV.9* %50, %class.KV.9** %18, align 8
  %51 = load i64, i64* %15, align 8
  %52 = load i64, i64* %14, align 8
  %53 = icmp ult i64 %51, %52
  br i1 %53, label %54, label %67

; <label>:54                                      ; preds = %48
  %55 = load %class.KV.9*, %class.KV.9** %18, align 8
  %56 = getelementptr inbounds %class.KV.9, %class.KV.9* %55, i64 0
  %57 = bitcast %class.KV.9* %56 to i8*
  %58 = bitcast i8* %57 to %class.KV.9*
  %59 = load %class.key*, %class.key** %12, align 8
  %60 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %58, %class.key* %59, %class.value* %60)
  %61 = load %class.KV.9*, %class.KV.9** %18, align 8
  %62 = getelementptr inbounds %class.KV.9, %class.KV.9* %61, i64 1
  %63 = bitcast %class.KV.9* %62 to i8*
  %64 = bitcast i8* %63 to %class.KV.9*
  %65 = load %class.key*, %class.key** %9, align 8
  %66 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %64, %class.key* %65, %class.value* %66)
  br label %80

; <label>:67                                      ; preds = %48
  %68 = load %class.KV.9*, %class.KV.9** %18, align 8
  %69 = getelementptr inbounds %class.KV.9, %class.KV.9* %68, i64 0
  %70 = bitcast %class.KV.9* %69 to i8*
  %71 = bitcast i8* %70 to %class.KV.9*
  %72 = load %class.key*, %class.key** %9, align 8
  %73 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %71, %class.key* %72, %class.value* %73)
  %74 = load %class.KV.9*, %class.KV.9** %18, align 8
  %75 = getelementptr inbounds %class.KV.9, %class.KV.9* %74, i64 1
  %76 = bitcast %class.KV.9* %75 to i8*
  %77 = bitcast i8* %76 to %class.KV.9*
  %78 = load %class.key*, %class.key** %12, align 8
  %79 = load %class.value*, %class.value** %13, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %77, %class.key* %78, %class.value* %79)
  br label %80

; <label>:80                                      ; preds = %67, %54
  %81 = load i64, i64* %14, align 8
  %82 = shl i64 1, %81
  %83 = load i64, i64* %15, align 8
  %84 = shl i64 1, %83
  %85 = or i64 %82, %84
  %86 = shl i64 %85, 1
  %87 = or i64 %86, 1
  %88 = load %class.KV.9*, %class.KV.9** %18, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %87, %class.KV.9* %88)
  br label %89

; <label>:89                                      ; preds = %80, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8*, %class.KV.8* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.8*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %class.KV.8*, %class.KV.8** %3, align 8
  %6 = load %class.KV.8*, %class.KV.8** %4, align 8
  call void @_ZN2KVI3key5valueLj9EEC2ERKS2_(%class.KV.8* %5, %class.KV.8* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7*, i64, %class.KV.8*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.8*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.8* %2, %class.KV.8** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3key5valueLj8EEC2EyPKS_IS0_S1_Lj9EE(%class.KV.7* %7, i64 %8, %class.KV.8* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj9EEC2EPKS0_PKS1_(%class.KV.8* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.9* noalias sret, i64, %class.key*, %class.value*, i64, %class.key*, %class.value*) #0 align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.value*, align 8
  %14 = alloca %class.LL*, align 8
  %15 = alloca %class.LL*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.value* %3, %class.value** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.value* %6, %class.value** %13, align 8
  %16 = call i8* @malloc(i64 24)
  %17 = bitcast i8* %16 to %class.LL*
  %18 = bitcast %class.LL* %17 to i8*
  %19 = bitcast i8* %18 to %class.LL*
  %20 = load %class.key*, %class.key** %9, align 8
  %21 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %19, %class.key* %20, %class.value* %21, %class.LL* null)
  store %class.LL* %19, %class.LL** %14, align 8
  %22 = call i8* @malloc(i64 24)
  %23 = bitcast i8* %22 to %class.LL*
  %24 = bitcast %class.LL* %23 to i8*
  %25 = bitcast i8* %24 to %class.LL*
  %26 = load %class.key*, %class.key** %12, align 8
  %27 = load %class.value*, %class.value** %13, align 8
  %28 = load %class.LL*, %class.LL** %14, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %25, %class.key* %26, %class.value* %27, %class.LL* %28)
  store %class.LL* %25, %class.LL** %15, align 8
  %29 = load %class.LL*, %class.LL** %15, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9* %0, i64 1, %class.LL* %29)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC1ERKS2_(%class.KV.9*, %class.KV.9* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV.9*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %class.KV.9*, %class.KV.9** %3, align 8
  %6 = load %class.KV.9*, %class.KV.9** %4, align 8
  call void @_ZN2KVI3key5valueLj10EEC2ERKS2_(%class.KV.9* %5, %class.KV.9* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8*, i64, %class.KV.9*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.9*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.9* %2, %class.KV.9** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3key5valueLj9EEC2EyPKS_IS0_S1_Lj10EE(%class.KV.8* %7, i64 %8, %class.KV.9* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = load %class.key*, %class.key** %5, align 8
  %9 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj10EEC2EPKS0_PKS1_(%class.KV.9* %7, %class.key* %8, %class.value* %9)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL*, %class.key*, %class.value*, %class.LL*) unnamed_addr #0 align 2 {
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca %class.value*, align 8
  %8 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %5, align 8
  store %class.key* %1, %class.key** %6, align 8
  store %class.value* %2, %class.value** %7, align 8
  store %class.LL* %3, %class.LL** %8, align 8
  %9 = load %class.LL*, %class.LL** %5, align 8
  %10 = load %class.key*, %class.key** %6, align 8
  %11 = load %class.value*, %class.value** %7, align 8
  %12 = load %class.LL*, %class.LL** %8, align 8
  call void @_ZN2LLI3key5valueEC2EPKS0_PKS1_PKS2_(%class.LL* %9, %class.key* %10, %class.value* %11, %class.LL* %12)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9*, i64, %class.LL*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.LL*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.LL* %2, %class.LL** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = load i64, i64* %5, align 8
  %9 = load %class.LL*, %class.LL** %6, align 8
  call void @_ZN2KVI3key5valueLj10EEC2EyPK2LLIS0_S1_E(%class.KV.9* %7, i64 %8, %class.LL* %9)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2LLI3key5valueEC2EPKS0_PKS1_PKS2_(%class.LL*, %class.key*, %class.value*, %class.LL*) unnamed_addr #5 align 2 {
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca %class.value*, align 8
  %8 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %5, align 8
  store %class.key* %1, %class.key** %6, align 8
  store %class.value* %2, %class.value** %7, align 8
  store %class.LL* %3, %class.LL** %8, align 8
  %9 = load %class.LL*, %class.LL** %5, align 8
  %10 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %11 = load %class.key*, %class.key** %6, align 8
  store %class.key* %11, %class.key** %10, align 8
  %12 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 1
  %13 = load %class.value*, %class.value** %7, align 8
  store %class.value* %13, %class.value** %12, align 8
  %14 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %15 = load %class.LL*, %class.LL** %8, align 8
  store %class.LL* %15, %class.LL** %14, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC2EyPK2LLIS0_S1_E(%class.KV.9*, i64, %class.LL*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.LL*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.LL* %2, %class.LL** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj10EE3KeyC1Ey(%"union.KV<key, value, 10>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 1
  %11 = load %class.LL*, %class.LL** %6, align 8
  call void @_ZN2KVI3key5valueLj10EE3ValC1EPK2LLIS0_S1_E(%"union.KV<key, value, 10>::Val"* %10, %class.LL* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3KeyC1Ey(%"union.KV<key, value, 10>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 10>::Key"* %0, %"union.KV<key, value, 10>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 10>::Key"*, %"union.KV<key, value, 10>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj10EE3KeyC2Ey(%"union.KV<key, value, 10>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3ValC1EPK2LLIS0_S1_E(%"union.KV<key, value, 10>::Val"*, %class.LL*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Val"*, align 8
  %4 = alloca %class.LL*, align 8
  store %"union.KV<key, value, 10>::Val"* %0, %"union.KV<key, value, 10>::Val"** %3, align 8
  store %class.LL* %1, %class.LL** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Val"*, %"union.KV<key, value, 10>::Val"** %3, align 8
  %6 = load %class.LL*, %class.LL** %4, align 8
  call void @_ZN2KVI3key5valueLj10EE3ValC2EPK2LLIS0_S1_E(%"union.KV<key, value, 10>::Val"* %5, %class.LL* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3KeyC2Ey(%"union.KV<key, value, 10>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 10>::Key"* %0, %"union.KV<key, value, 10>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 10>::Key"*, %"union.KV<key, value, 10>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 10>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3ValC2EPK2LLIS0_S1_E(%"union.KV<key, value, 10>::Val"*, %class.LL*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Val"*, align 8
  %4 = alloca %class.LL*, align 8
  store %"union.KV<key, value, 10>::Val"* %0, %"union.KV<key, value, 10>::Val"** %3, align 8
  store %class.LL* %1, %class.LL** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Val"*, %"union.KV<key, value, 10>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 10>::Val"* %5 to %class.LL**
  %7 = load %class.LL*, %class.LL** %4, align 8
  store %class.LL* %7, %class.LL** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC2ERKS2_(%class.KV.9*, %class.KV.9* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.9*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %class.KV.9*, %class.KV.9** %3, align 8
  %6 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 0
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 10>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 10>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 1
  %12 = load %class.KV.9*, %class.KV.9** %4, align 8
  %13 = getelementptr inbounds %class.KV.9, %class.KV.9* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 10>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 10>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC2EyPKS_IS0_S1_Lj10EE(%class.KV.8*, i64, %class.KV.9*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.9*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.9* %2, %class.KV.9** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj9EE3KeyC1Ey(%"union.KV<key, value, 9>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 1
  %11 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3key5valueLj9EE3ValC1EPKS_IS0_S1_Lj10EE(%"union.KV<key, value, 9>::Val"* %10, %class.KV.9* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3KeyC1Ey(%"union.KV<key, value, 9>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 9>::Key"* %0, %"union.KV<key, value, 9>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 9>::Key"*, %"union.KV<key, value, 9>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj9EE3KeyC2Ey(%"union.KV<key, value, 9>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3ValC1EPKS_IS0_S1_Lj10EE(%"union.KV<key, value, 9>::Val"*, %class.KV.9*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Val"*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %"union.KV<key, value, 9>::Val"* %0, %"union.KV<key, value, 9>::Val"** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Val"*, %"union.KV<key, value, 9>::Val"** %3, align 8
  %6 = load %class.KV.9*, %class.KV.9** %4, align 8
  call void @_ZN2KVI3key5valueLj9EE3ValC2EPKS_IS0_S1_Lj10EE(%"union.KV<key, value, 9>::Val"* %5, %class.KV.9* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3KeyC2Ey(%"union.KV<key, value, 9>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 9>::Key"* %0, %"union.KV<key, value, 9>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 9>::Key"*, %"union.KV<key, value, 9>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 9>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3ValC2EPKS_IS0_S1_Lj10EE(%"union.KV<key, value, 9>::Val"*, %class.KV.9*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Val"*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %"union.KV<key, value, 9>::Val"* %0, %"union.KV<key, value, 9>::Val"** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Val"*, %"union.KV<key, value, 9>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 9>::Val"* %5 to %class.KV.9**
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  store %class.KV.9* %7, %class.KV.9** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EEC2EPKS0_PKS1_(%class.KV.9*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj10EE3KeyC1EPKS0_(%"union.KV<key, value, 10>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj10EE3ValC1EPKS1_(%"union.KV<key, value, 10>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3KeyC1EPKS0_(%"union.KV<key, value, 10>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 10>::Key"* %0, %"union.KV<key, value, 10>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Key"*, %"union.KV<key, value, 10>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj10EE3KeyC2EPKS0_(%"union.KV<key, value, 10>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3ValC1EPKS1_(%"union.KV<key, value, 10>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 10>::Val"* %0, %"union.KV<key, value, 10>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Val"*, %"union.KV<key, value, 10>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj10EE3ValC2EPKS1_(%"union.KV<key, value, 10>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3KeyC2EPKS0_(%"union.KV<key, value, 10>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 10>::Key"* %0, %"union.KV<key, value, 10>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Key"*, %"union.KV<key, value, 10>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 10>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE3ValC2EPKS1_(%"union.KV<key, value, 10>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 10>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 10>::Val"* %0, %"union.KV<key, value, 10>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 10>::Val"*, %"union.KV<key, value, 10>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 10>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC2ERKS2_(%class.KV.8*, %class.KV.8* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.8*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %class.KV.8*, %class.KV.8** %3, align 8
  %6 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 0
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 9>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 9>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 1
  %12 = load %class.KV.8*, %class.KV.8** %4, align 8
  %13 = getelementptr inbounds %class.KV.8, %class.KV.8* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 9>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 9>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC2EyPKS_IS0_S1_Lj9EE(%class.KV.7*, i64, %class.KV.8*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.8*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.8* %2, %class.KV.8** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj8EE3KeyC1Ey(%"union.KV<key, value, 8>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 1
  %11 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3key5valueLj8EE3ValC1EPKS_IS0_S1_Lj9EE(%"union.KV<key, value, 8>::Val"* %10, %class.KV.8* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3KeyC1Ey(%"union.KV<key, value, 8>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 8>::Key"* %0, %"union.KV<key, value, 8>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 8>::Key"*, %"union.KV<key, value, 8>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj8EE3KeyC2Ey(%"union.KV<key, value, 8>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3ValC1EPKS_IS0_S1_Lj9EE(%"union.KV<key, value, 8>::Val"*, %class.KV.8*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Val"*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %"union.KV<key, value, 8>::Val"* %0, %"union.KV<key, value, 8>::Val"** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Val"*, %"union.KV<key, value, 8>::Val"** %3, align 8
  %6 = load %class.KV.8*, %class.KV.8** %4, align 8
  call void @_ZN2KVI3key5valueLj8EE3ValC2EPKS_IS0_S1_Lj9EE(%"union.KV<key, value, 8>::Val"* %5, %class.KV.8* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3KeyC2Ey(%"union.KV<key, value, 8>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 8>::Key"* %0, %"union.KV<key, value, 8>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 8>::Key"*, %"union.KV<key, value, 8>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 8>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3ValC2EPKS_IS0_S1_Lj9EE(%"union.KV<key, value, 8>::Val"*, %class.KV.8*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Val"*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %"union.KV<key, value, 8>::Val"* %0, %"union.KV<key, value, 8>::Val"** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Val"*, %"union.KV<key, value, 8>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 8>::Val"* %5 to %class.KV.8**
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  store %class.KV.8* %7, %class.KV.8** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EEC2EPKS0_PKS1_(%class.KV.8*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj9EE3KeyC1EPKS0_(%"union.KV<key, value, 9>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj9EE3ValC1EPKS1_(%"union.KV<key, value, 9>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3KeyC1EPKS0_(%"union.KV<key, value, 9>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 9>::Key"* %0, %"union.KV<key, value, 9>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Key"*, %"union.KV<key, value, 9>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj9EE3KeyC2EPKS0_(%"union.KV<key, value, 9>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3ValC1EPKS1_(%"union.KV<key, value, 9>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 9>::Val"* %0, %"union.KV<key, value, 9>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Val"*, %"union.KV<key, value, 9>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj9EE3ValC2EPKS1_(%"union.KV<key, value, 9>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3KeyC2EPKS0_(%"union.KV<key, value, 9>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 9>::Key"* %0, %"union.KV<key, value, 9>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Key"*, %"union.KV<key, value, 9>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 9>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE3ValC2EPKS1_(%"union.KV<key, value, 9>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 9>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 9>::Val"* %0, %"union.KV<key, value, 9>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 9>::Val"*, %"union.KV<key, value, 9>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 9>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC2ERKS2_(%class.KV.7*, %class.KV.7* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.7*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %class.KV.7*, %class.KV.7** %3, align 8
  %6 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 0
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 8>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 8>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 1
  %12 = load %class.KV.7*, %class.KV.7** %4, align 8
  %13 = getelementptr inbounds %class.KV.7, %class.KV.7* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 8>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 8>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC2EyPKS_IS0_S1_Lj8EE(%class.KV.6*, i64, %class.KV.7*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.7*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.7* %2, %class.KV.7** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj7EE3KeyC1Ey(%"union.KV<key, value, 7>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 1
  %11 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3key5valueLj7EE3ValC1EPKS_IS0_S1_Lj8EE(%"union.KV<key, value, 7>::Val"* %10, %class.KV.7* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3KeyC1Ey(%"union.KV<key, value, 7>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 7>::Key"* %0, %"union.KV<key, value, 7>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 7>::Key"*, %"union.KV<key, value, 7>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj7EE3KeyC2Ey(%"union.KV<key, value, 7>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3ValC1EPKS_IS0_S1_Lj8EE(%"union.KV<key, value, 7>::Val"*, %class.KV.7*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Val"*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %"union.KV<key, value, 7>::Val"* %0, %"union.KV<key, value, 7>::Val"** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Val"*, %"union.KV<key, value, 7>::Val"** %3, align 8
  %6 = load %class.KV.7*, %class.KV.7** %4, align 8
  call void @_ZN2KVI3key5valueLj7EE3ValC2EPKS_IS0_S1_Lj8EE(%"union.KV<key, value, 7>::Val"* %5, %class.KV.7* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3KeyC2Ey(%"union.KV<key, value, 7>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 7>::Key"* %0, %"union.KV<key, value, 7>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 7>::Key"*, %"union.KV<key, value, 7>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 7>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3ValC2EPKS_IS0_S1_Lj8EE(%"union.KV<key, value, 7>::Val"*, %class.KV.7*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Val"*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %"union.KV<key, value, 7>::Val"* %0, %"union.KV<key, value, 7>::Val"** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Val"*, %"union.KV<key, value, 7>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 7>::Val"* %5 to %class.KV.7**
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  store %class.KV.7* %7, %class.KV.7** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EEC2EPKS0_PKS1_(%class.KV.7*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj8EE3KeyC1EPKS0_(%"union.KV<key, value, 8>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj8EE3ValC1EPKS1_(%"union.KV<key, value, 8>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3KeyC1EPKS0_(%"union.KV<key, value, 8>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 8>::Key"* %0, %"union.KV<key, value, 8>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Key"*, %"union.KV<key, value, 8>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj8EE3KeyC2EPKS0_(%"union.KV<key, value, 8>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3ValC1EPKS1_(%"union.KV<key, value, 8>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 8>::Val"* %0, %"union.KV<key, value, 8>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Val"*, %"union.KV<key, value, 8>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj8EE3ValC2EPKS1_(%"union.KV<key, value, 8>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3KeyC2EPKS0_(%"union.KV<key, value, 8>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 8>::Key"* %0, %"union.KV<key, value, 8>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Key"*, %"union.KV<key, value, 8>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 8>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE3ValC2EPKS1_(%"union.KV<key, value, 8>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 8>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 8>::Val"* %0, %"union.KV<key, value, 8>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 8>::Val"*, %"union.KV<key, value, 8>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 8>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC2ERKS2_(%class.KV.6*, %class.KV.6* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.6*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %class.KV.6*, %class.KV.6** %3, align 8
  %6 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 0
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 7>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 7>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 1
  %12 = load %class.KV.6*, %class.KV.6** %4, align 8
  %13 = getelementptr inbounds %class.KV.6, %class.KV.6* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 7>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 7>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC2EyPKS_IS0_S1_Lj7EE(%class.KV.5*, i64, %class.KV.6*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.6*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.6* %2, %class.KV.6** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj6EE3KeyC1Ey(%"union.KV<key, value, 6>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 1
  %11 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3key5valueLj6EE3ValC1EPKS_IS0_S1_Lj7EE(%"union.KV<key, value, 6>::Val"* %10, %class.KV.6* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3KeyC1Ey(%"union.KV<key, value, 6>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 6>::Key"* %0, %"union.KV<key, value, 6>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 6>::Key"*, %"union.KV<key, value, 6>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj6EE3KeyC2Ey(%"union.KV<key, value, 6>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3ValC1EPKS_IS0_S1_Lj7EE(%"union.KV<key, value, 6>::Val"*, %class.KV.6*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Val"*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %"union.KV<key, value, 6>::Val"* %0, %"union.KV<key, value, 6>::Val"** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Val"*, %"union.KV<key, value, 6>::Val"** %3, align 8
  %6 = load %class.KV.6*, %class.KV.6** %4, align 8
  call void @_ZN2KVI3key5valueLj6EE3ValC2EPKS_IS0_S1_Lj7EE(%"union.KV<key, value, 6>::Val"* %5, %class.KV.6* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3KeyC2Ey(%"union.KV<key, value, 6>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 6>::Key"* %0, %"union.KV<key, value, 6>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 6>::Key"*, %"union.KV<key, value, 6>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 6>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3ValC2EPKS_IS0_S1_Lj7EE(%"union.KV<key, value, 6>::Val"*, %class.KV.6*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Val"*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %"union.KV<key, value, 6>::Val"* %0, %"union.KV<key, value, 6>::Val"** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Val"*, %"union.KV<key, value, 6>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 6>::Val"* %5 to %class.KV.6**
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  store %class.KV.6* %7, %class.KV.6** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EEC2EPKS0_PKS1_(%class.KV.6*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj7EE3KeyC1EPKS0_(%"union.KV<key, value, 7>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj7EE3ValC1EPKS1_(%"union.KV<key, value, 7>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3KeyC1EPKS0_(%"union.KV<key, value, 7>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 7>::Key"* %0, %"union.KV<key, value, 7>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Key"*, %"union.KV<key, value, 7>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj7EE3KeyC2EPKS0_(%"union.KV<key, value, 7>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3ValC1EPKS1_(%"union.KV<key, value, 7>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 7>::Val"* %0, %"union.KV<key, value, 7>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Val"*, %"union.KV<key, value, 7>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj7EE3ValC2EPKS1_(%"union.KV<key, value, 7>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3KeyC2EPKS0_(%"union.KV<key, value, 7>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 7>::Key"* %0, %"union.KV<key, value, 7>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Key"*, %"union.KV<key, value, 7>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 7>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE3ValC2EPKS1_(%"union.KV<key, value, 7>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 7>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 7>::Val"* %0, %"union.KV<key, value, 7>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 7>::Val"*, %"union.KV<key, value, 7>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 7>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC2ERKS2_(%class.KV.5*, %class.KV.5* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.5*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %class.KV.5*, %class.KV.5** %3, align 8
  %6 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 0
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 6>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 6>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 1
  %12 = load %class.KV.5*, %class.KV.5** %4, align 8
  %13 = getelementptr inbounds %class.KV.5, %class.KV.5* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 6>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 6>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC2EyPKS_IS0_S1_Lj6EE(%class.KV.4*, i64, %class.KV.5*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.5*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.5* %2, %class.KV.5** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj5EE3KeyC1Ey(%"union.KV<key, value, 5>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 1
  %11 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3key5valueLj5EE3ValC1EPKS_IS0_S1_Lj6EE(%"union.KV<key, value, 5>::Val"* %10, %class.KV.5* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3KeyC1Ey(%"union.KV<key, value, 5>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 5>::Key"* %0, %"union.KV<key, value, 5>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 5>::Key"*, %"union.KV<key, value, 5>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj5EE3KeyC2Ey(%"union.KV<key, value, 5>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3ValC1EPKS_IS0_S1_Lj6EE(%"union.KV<key, value, 5>::Val"*, %class.KV.5*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Val"*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %"union.KV<key, value, 5>::Val"* %0, %"union.KV<key, value, 5>::Val"** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Val"*, %"union.KV<key, value, 5>::Val"** %3, align 8
  %6 = load %class.KV.5*, %class.KV.5** %4, align 8
  call void @_ZN2KVI3key5valueLj5EE3ValC2EPKS_IS0_S1_Lj6EE(%"union.KV<key, value, 5>::Val"* %5, %class.KV.5* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3KeyC2Ey(%"union.KV<key, value, 5>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 5>::Key"* %0, %"union.KV<key, value, 5>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 5>::Key"*, %"union.KV<key, value, 5>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 5>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3ValC2EPKS_IS0_S1_Lj6EE(%"union.KV<key, value, 5>::Val"*, %class.KV.5*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Val"*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %"union.KV<key, value, 5>::Val"* %0, %"union.KV<key, value, 5>::Val"** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Val"*, %"union.KV<key, value, 5>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 5>::Val"* %5 to %class.KV.5**
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  store %class.KV.5* %7, %class.KV.5** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EEC2EPKS0_PKS1_(%class.KV.5*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj6EE3KeyC1EPKS0_(%"union.KV<key, value, 6>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj6EE3ValC1EPKS1_(%"union.KV<key, value, 6>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3KeyC1EPKS0_(%"union.KV<key, value, 6>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 6>::Key"* %0, %"union.KV<key, value, 6>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Key"*, %"union.KV<key, value, 6>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj6EE3KeyC2EPKS0_(%"union.KV<key, value, 6>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3ValC1EPKS1_(%"union.KV<key, value, 6>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 6>::Val"* %0, %"union.KV<key, value, 6>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Val"*, %"union.KV<key, value, 6>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj6EE3ValC2EPKS1_(%"union.KV<key, value, 6>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3KeyC2EPKS0_(%"union.KV<key, value, 6>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 6>::Key"* %0, %"union.KV<key, value, 6>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Key"*, %"union.KV<key, value, 6>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 6>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE3ValC2EPKS1_(%"union.KV<key, value, 6>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 6>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 6>::Val"* %0, %"union.KV<key, value, 6>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 6>::Val"*, %"union.KV<key, value, 6>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 6>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC2ERKS2_(%class.KV.4*, %class.KV.4* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.4*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %class.KV.4*, %class.KV.4** %3, align 8
  %6 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 0
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 5>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 5>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 1
  %12 = load %class.KV.4*, %class.KV.4** %4, align 8
  %13 = getelementptr inbounds %class.KV.4, %class.KV.4* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 5>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 5>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC2EyPKS_IS0_S1_Lj5EE(%class.KV.3*, i64, %class.KV.4*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.4*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.4* %2, %class.KV.4** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj4EE3KeyC1Ey(%"union.KV<key, value, 4>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 1
  %11 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3key5valueLj4EE3ValC1EPKS_IS0_S1_Lj5EE(%"union.KV<key, value, 4>::Val"* %10, %class.KV.4* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3KeyC1Ey(%"union.KV<key, value, 4>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 4>::Key"* %0, %"union.KV<key, value, 4>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 4>::Key"*, %"union.KV<key, value, 4>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj4EE3KeyC2Ey(%"union.KV<key, value, 4>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3ValC1EPKS_IS0_S1_Lj5EE(%"union.KV<key, value, 4>::Val"*, %class.KV.4*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Val"*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %"union.KV<key, value, 4>::Val"* %0, %"union.KV<key, value, 4>::Val"** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Val"*, %"union.KV<key, value, 4>::Val"** %3, align 8
  %6 = load %class.KV.4*, %class.KV.4** %4, align 8
  call void @_ZN2KVI3key5valueLj4EE3ValC2EPKS_IS0_S1_Lj5EE(%"union.KV<key, value, 4>::Val"* %5, %class.KV.4* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3KeyC2Ey(%"union.KV<key, value, 4>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 4>::Key"* %0, %"union.KV<key, value, 4>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 4>::Key"*, %"union.KV<key, value, 4>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 4>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3ValC2EPKS_IS0_S1_Lj5EE(%"union.KV<key, value, 4>::Val"*, %class.KV.4*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Val"*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %"union.KV<key, value, 4>::Val"* %0, %"union.KV<key, value, 4>::Val"** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Val"*, %"union.KV<key, value, 4>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 4>::Val"* %5 to %class.KV.4**
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  store %class.KV.4* %7, %class.KV.4** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EEC2EPKS0_PKS1_(%class.KV.4*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj5EE3KeyC1EPKS0_(%"union.KV<key, value, 5>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj5EE3ValC1EPKS1_(%"union.KV<key, value, 5>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3KeyC1EPKS0_(%"union.KV<key, value, 5>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 5>::Key"* %0, %"union.KV<key, value, 5>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Key"*, %"union.KV<key, value, 5>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj5EE3KeyC2EPKS0_(%"union.KV<key, value, 5>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3ValC1EPKS1_(%"union.KV<key, value, 5>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 5>::Val"* %0, %"union.KV<key, value, 5>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Val"*, %"union.KV<key, value, 5>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj5EE3ValC2EPKS1_(%"union.KV<key, value, 5>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3KeyC2EPKS0_(%"union.KV<key, value, 5>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 5>::Key"* %0, %"union.KV<key, value, 5>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Key"*, %"union.KV<key, value, 5>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 5>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE3ValC2EPKS1_(%"union.KV<key, value, 5>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 5>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 5>::Val"* %0, %"union.KV<key, value, 5>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 5>::Val"*, %"union.KV<key, value, 5>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 5>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC2ERKS2_(%class.KV.3*, %class.KV.3* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.3*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %class.KV.3*, %class.KV.3** %3, align 8
  %6 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 0
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 4>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 4>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 1
  %12 = load %class.KV.3*, %class.KV.3** %4, align 8
  %13 = getelementptr inbounds %class.KV.3, %class.KV.3* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 4>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 4>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC2EyPKS_IS0_S1_Lj4EE(%class.KV.2*, i64, %class.KV.3*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.3*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.3* %2, %class.KV.3** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj3EE3KeyC1Ey(%"union.KV<key, value, 3>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 1
  %11 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3key5valueLj3EE3ValC1EPKS_IS0_S1_Lj4EE(%"union.KV<key, value, 3>::Val"* %10, %class.KV.3* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3KeyC1Ey(%"union.KV<key, value, 3>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 3>::Key"* %0, %"union.KV<key, value, 3>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 3>::Key"*, %"union.KV<key, value, 3>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj3EE3KeyC2Ey(%"union.KV<key, value, 3>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3ValC1EPKS_IS0_S1_Lj4EE(%"union.KV<key, value, 3>::Val"*, %class.KV.3*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Val"*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %"union.KV<key, value, 3>::Val"* %0, %"union.KV<key, value, 3>::Val"** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Val"*, %"union.KV<key, value, 3>::Val"** %3, align 8
  %6 = load %class.KV.3*, %class.KV.3** %4, align 8
  call void @_ZN2KVI3key5valueLj3EE3ValC2EPKS_IS0_S1_Lj4EE(%"union.KV<key, value, 3>::Val"* %5, %class.KV.3* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3KeyC2Ey(%"union.KV<key, value, 3>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 3>::Key"* %0, %"union.KV<key, value, 3>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 3>::Key"*, %"union.KV<key, value, 3>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 3>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3ValC2EPKS_IS0_S1_Lj4EE(%"union.KV<key, value, 3>::Val"*, %class.KV.3*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Val"*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %"union.KV<key, value, 3>::Val"* %0, %"union.KV<key, value, 3>::Val"** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Val"*, %"union.KV<key, value, 3>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 3>::Val"* %5 to %class.KV.3**
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  store %class.KV.3* %7, %class.KV.3** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EEC2EPKS0_PKS1_(%class.KV.3*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj4EE3KeyC1EPKS0_(%"union.KV<key, value, 4>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj4EE3ValC1EPKS1_(%"union.KV<key, value, 4>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3KeyC1EPKS0_(%"union.KV<key, value, 4>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 4>::Key"* %0, %"union.KV<key, value, 4>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Key"*, %"union.KV<key, value, 4>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj4EE3KeyC2EPKS0_(%"union.KV<key, value, 4>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3ValC1EPKS1_(%"union.KV<key, value, 4>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 4>::Val"* %0, %"union.KV<key, value, 4>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Val"*, %"union.KV<key, value, 4>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj4EE3ValC2EPKS1_(%"union.KV<key, value, 4>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3KeyC2EPKS0_(%"union.KV<key, value, 4>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 4>::Key"* %0, %"union.KV<key, value, 4>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Key"*, %"union.KV<key, value, 4>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 4>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE3ValC2EPKS1_(%"union.KV<key, value, 4>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 4>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 4>::Val"* %0, %"union.KV<key, value, 4>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 4>::Val"*, %"union.KV<key, value, 4>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 4>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC2ERKS2_(%class.KV.2*, %class.KV.2* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.2*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %class.KV.2*, %class.KV.2** %3, align 8
  %6 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 0
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 3>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 3>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 1
  %12 = load %class.KV.2*, %class.KV.2** %4, align 8
  %13 = getelementptr inbounds %class.KV.2, %class.KV.2* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 3>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 3>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC2EyPKS_IS0_S1_Lj3EE(%class.KV.1*, i64, %class.KV.2*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.2*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.2* %2, %class.KV.2** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj2EE3KeyC1Ey(%"union.KV<key, value, 2>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 1
  %11 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3key5valueLj2EE3ValC1EPKS_IS0_S1_Lj3EE(%"union.KV<key, value, 2>::Val"* %10, %class.KV.2* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3KeyC1Ey(%"union.KV<key, value, 2>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 2>::Key"* %0, %"union.KV<key, value, 2>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 2>::Key"*, %"union.KV<key, value, 2>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj2EE3KeyC2Ey(%"union.KV<key, value, 2>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3ValC1EPKS_IS0_S1_Lj3EE(%"union.KV<key, value, 2>::Val"*, %class.KV.2*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Val"*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %"union.KV<key, value, 2>::Val"* %0, %"union.KV<key, value, 2>::Val"** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Val"*, %"union.KV<key, value, 2>::Val"** %3, align 8
  %6 = load %class.KV.2*, %class.KV.2** %4, align 8
  call void @_ZN2KVI3key5valueLj2EE3ValC2EPKS_IS0_S1_Lj3EE(%"union.KV<key, value, 2>::Val"* %5, %class.KV.2* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3KeyC2Ey(%"union.KV<key, value, 2>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 2>::Key"* %0, %"union.KV<key, value, 2>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 2>::Key"*, %"union.KV<key, value, 2>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 2>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3ValC2EPKS_IS0_S1_Lj3EE(%"union.KV<key, value, 2>::Val"*, %class.KV.2*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Val"*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %"union.KV<key, value, 2>::Val"* %0, %"union.KV<key, value, 2>::Val"** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Val"*, %"union.KV<key, value, 2>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 2>::Val"* %5 to %class.KV.2**
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  store %class.KV.2* %7, %class.KV.2** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EEC2EPKS0_PKS1_(%class.KV.2*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj3EE3KeyC1EPKS0_(%"union.KV<key, value, 3>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj3EE3ValC1EPKS1_(%"union.KV<key, value, 3>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3KeyC1EPKS0_(%"union.KV<key, value, 3>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 3>::Key"* %0, %"union.KV<key, value, 3>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Key"*, %"union.KV<key, value, 3>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj3EE3KeyC2EPKS0_(%"union.KV<key, value, 3>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3ValC1EPKS1_(%"union.KV<key, value, 3>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 3>::Val"* %0, %"union.KV<key, value, 3>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Val"*, %"union.KV<key, value, 3>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj3EE3ValC2EPKS1_(%"union.KV<key, value, 3>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3KeyC2EPKS0_(%"union.KV<key, value, 3>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 3>::Key"* %0, %"union.KV<key, value, 3>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Key"*, %"union.KV<key, value, 3>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 3>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE3ValC2EPKS1_(%"union.KV<key, value, 3>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 3>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 3>::Val"* %0, %"union.KV<key, value, 3>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 3>::Val"*, %"union.KV<key, value, 3>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 3>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC2ERKS2_(%class.KV.1*, %class.KV.1* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.1*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %class.KV.1*, %class.KV.1** %3, align 8
  %6 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 0
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 2>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 2>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 1
  %12 = load %class.KV.1*, %class.KV.1** %4, align 8
  %13 = getelementptr inbounds %class.KV.1, %class.KV.1* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 2>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 2>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC2EyPKS_IS0_S1_Lj2EE(%class.KV.0*, i64, %class.KV.1*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.1*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.1* %2, %class.KV.1** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj1EE3KeyC1Ey(%"union.KV<key, value, 1>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 1
  %11 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3key5valueLj1EE3ValC1EPKS_IS0_S1_Lj2EE(%"union.KV<key, value, 1>::Val"* %10, %class.KV.1* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3KeyC1Ey(%"union.KV<key, value, 1>::Key"*, i64) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 1>::Key"* %0, %"union.KV<key, value, 1>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 1>::Key"*, %"union.KV<key, value, 1>::Key"** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @_ZN2KVI3key5valueLj1EE3KeyC2Ey(%"union.KV<key, value, 1>::Key"* %5, i64 %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3ValC1EPKS_IS0_S1_Lj2EE(%"union.KV<key, value, 1>::Val"*, %class.KV.1*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Val"*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %"union.KV<key, value, 1>::Val"* %0, %"union.KV<key, value, 1>::Val"** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Val"*, %"union.KV<key, value, 1>::Val"** %3, align 8
  %6 = load %class.KV.1*, %class.KV.1** %4, align 8
  call void @_ZN2KVI3key5valueLj1EE3ValC2EPKS_IS0_S1_Lj2EE(%"union.KV<key, value, 1>::Val"* %5, %class.KV.1* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3KeyC2Ey(%"union.KV<key, value, 1>::Key"*, i64) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, value, 1>::Key"* %0, %"union.KV<key, value, 1>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, value, 1>::Key"*, %"union.KV<key, value, 1>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 1>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3ValC2EPKS_IS0_S1_Lj2EE(%"union.KV<key, value, 1>::Val"*, %class.KV.1*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Val"*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %"union.KV<key, value, 1>::Val"* %0, %"union.KV<key, value, 1>::Val"** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Val"*, %"union.KV<key, value, 1>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 1>::Val"* %5 to %class.KV.1**
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  store %class.KV.1* %7, %class.KV.1** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EEC2EPKS0_PKS1_(%class.KV.1*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj2EE3KeyC1EPKS0_(%"union.KV<key, value, 2>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj2EE3ValC1EPKS1_(%"union.KV<key, value, 2>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3KeyC1EPKS0_(%"union.KV<key, value, 2>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 2>::Key"* %0, %"union.KV<key, value, 2>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Key"*, %"union.KV<key, value, 2>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj2EE3KeyC2EPKS0_(%"union.KV<key, value, 2>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3ValC1EPKS1_(%"union.KV<key, value, 2>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 2>::Val"* %0, %"union.KV<key, value, 2>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Val"*, %"union.KV<key, value, 2>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj2EE3ValC2EPKS1_(%"union.KV<key, value, 2>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3KeyC2EPKS0_(%"union.KV<key, value, 2>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 2>::Key"* %0, %"union.KV<key, value, 2>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Key"*, %"union.KV<key, value, 2>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 2>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE3ValC2EPKS1_(%"union.KV<key, value, 2>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 2>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 2>::Val"* %0, %"union.KV<key, value, 2>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 2>::Val"*, %"union.KV<key, value, 2>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 2>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC2ERKS2_(%class.KV.0*, %class.KV.0* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV.0*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %class.KV.0*, %class.KV.0** %3, align 8
  %6 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 0
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 1>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 1>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 1
  %12 = load %class.KV.0*, %class.KV.0** %4, align 8
  %13 = getelementptr inbounds %class.KV.0, %class.KV.0* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 1>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 1>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC2EyPKS_IS0_S1_Lj1EE(%class.KV*, i64, %class.KV.0*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.0*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.0* %2, %class.KV.0** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3key5valueLj0EE3KeyC1Ey(%"union.KV<key, value, 0>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 1
  %11 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3key5valueLj0EE3ValC1EPKS_IS0_S1_Lj1EE(%"union.KV<key, value, 0>::Val"* %10, %class.KV.0* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3ValC1EPKS_IS0_S1_Lj1EE(%"union.KV<key, value, 0>::Val"*, %class.KV.0*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Val"*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %"union.KV<key, value, 0>::Val"* %0, %"union.KV<key, value, 0>::Val"** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Val"*, %"union.KV<key, value, 0>::Val"** %3, align 8
  %6 = load %class.KV.0*, %class.KV.0** %4, align 8
  call void @_ZN2KVI3key5valueLj0EE3ValC2EPKS_IS0_S1_Lj1EE(%"union.KV<key, value, 0>::Val"* %5, %class.KV.0* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE3ValC2EPKS_IS0_S1_Lj1EE(%"union.KV<key, value, 0>::Val"*, %class.KV.0*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 0>::Val"*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %"union.KV<key, value, 0>::Val"* %0, %"union.KV<key, value, 0>::Val"** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %"union.KV<key, value, 0>::Val"*, %"union.KV<key, value, 0>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 0>::Val"* %5 to %class.KV.0**
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  store %class.KV.0* %7, %class.KV.0** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EEC2EPKS0_PKS1_(%class.KV.0*, %class.key*, %class.value*) unnamed_addr #0 align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.value*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.value* %2, %class.value** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3key5valueLj1EE3KeyC1EPKS0_(%"union.KV<key, value, 1>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 1
  %11 = load %class.value*, %class.value** %6, align 8
  call void @_ZN2KVI3key5valueLj1EE3ValC1EPKS1_(%"union.KV<key, value, 1>::Val"* %10, %class.value* %11)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3KeyC1EPKS0_(%"union.KV<key, value, 1>::Key"*, %class.key*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 1>::Key"* %0, %"union.KV<key, value, 1>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Key"*, %"union.KV<key, value, 1>::Key"** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  call void @_ZN2KVI3key5valueLj1EE3KeyC2EPKS0_(%"union.KV<key, value, 1>::Key"* %5, %class.key* %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3ValC1EPKS1_(%"union.KV<key, value, 1>::Val"*, %class.value*) unnamed_addr #0 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 1>::Val"* %0, %"union.KV<key, value, 1>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Val"*, %"union.KV<key, value, 1>::Val"** %3, align 8
  %6 = load %class.value*, %class.value** %4, align 8
  call void @_ZN2KVI3key5valueLj1EE3ValC2EPKS1_(%"union.KV<key, value, 1>::Val"* %5, %class.value* %6)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3KeyC2EPKS0_(%"union.KV<key, value, 1>::Key"*, %class.key*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, value, 1>::Key"* %0, %"union.KV<key, value, 1>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Key"*, %"union.KV<key, value, 1>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 1>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE3ValC2EPKS1_(%"union.KV<key, value, 1>::Val"*, %class.value*) unnamed_addr #5 align 2 {
  %3 = alloca %"union.KV<key, value, 1>::Val"*, align 8
  %4 = alloca %class.value*, align 8
  store %"union.KV<key, value, 1>::Val"* %0, %"union.KV<key, value, 1>::Val"** %3, align 8
  store %class.value* %1, %class.value** %4, align 8
  %5 = load %"union.KV<key, value, 1>::Val"*, %"union.KV<key, value, 1>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, value, 1>::Val"* %5 to %class.value**
  %7 = load %class.value*, %class.value** %4, align 8
  store %class.value* %7, %class.value** %6, align 8
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.0* @_ZN2KVI3key5valueLj1EE11update_nodeEPKS2_mmRS3_(%class.KV.0*, i64, i64, %class.KV.0* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.0*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.0*, align 8
  %9 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.0* %3, %class.KV.0** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.0*
  store %class.KV.0* %13, %class.KV.0** %9, align 8
  %14 = load %class.KV.0*, %class.KV.0** %9, align 8
  %15 = bitcast %class.KV.0* %14 to i8*
  %16 = load %class.KV.0*, %class.KV.0** %5, align 8
  %17 = bitcast %class.KV.0* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.0*, %class.KV.0** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.0, %class.KV.0* %20, i64 %21
  %23 = bitcast %class.KV.0* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.0*
  %25 = load %class.KV.0*, %class.KV.0** %8, align 8
  call void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0* %24, %class.KV.0* dereferenceable(16) %25)
  %26 = load %class.KV.0*, %class.KV.0** %9, align 8
  ret %class.KV.0* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.0* noalias sret, %class.KV.0* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.0*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.1*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.1*, align 8
  %19 = alloca %class.KV.1, align 8
  %20 = alloca %class.KV.1, align 8
  %21 = alloca %class.KV.1*, align 8
  %22 = alloca %class.KV.1, align 8
  %23 = alloca %class.KV.1*, align 8
  %24 = alloca %class.KV.1*, align 8
  store %class.KV.0* %1, %class.KV.0** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.0*, %class.KV.0** %7, align 8
  %26 = getelementptr inbounds %class.KV.0, %class.KV.0* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 1>::Val"* %26 to %class.KV.1**
  %28 = load %class.KV.1*, %class.KV.1** %27, align 8
  store %class.KV.1* %28, %class.KV.1** %12, align 8
  %29 = load %class.KV.0*, %class.KV.0** %7, align 8
  %30 = getelementptr inbounds %class.KV.0, %class.KV.0* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 1>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.1*, %class.KV.1** %12, align 8
  %60 = getelementptr inbounds %class.KV.1, %class.KV.1* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.1, %class.KV.1* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 2>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.1*, %class.KV.1** %12, align 8
  %69 = getelementptr inbounds %class.KV.1, %class.KV.1* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.1, %class.KV.1* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 2>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.1*, %class.KV.1** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.1* @_ZN2KVI3key5valueLj2EE11update_nodeEPKS2_mmRS3_(%class.KV.1* %76, i64 %77, i64 %78, %class.KV.1* dereferenceable(16) %19)
  store %class.KV.1* %81, %class.KV.1** %18, align 8
  %82 = load %class.KV.0*, %class.KV.0** %7, align 8
  %83 = getelementptr inbounds %class.KV.0, %class.KV.0* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 1>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.1*, %class.KV.1** %18, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %85, %class.KV.1* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.1*, %class.KV.1** %12, align 8
  %93 = getelementptr inbounds %class.KV.1, %class.KV.1* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.1, %class.KV.1* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 2>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 16
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.1*, %class.KV.1** %12, align 8
  %101 = getelementptr inbounds %class.KV.1, %class.KV.1* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.1, %class.KV.1* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 2>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.1*, %class.KV.1** %12, align 8
  %107 = getelementptr inbounds %class.KV.1, %class.KV.1* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.1, %class.KV.1* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 2>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj2EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.1* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.1*, %class.KV.1** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.1* @_ZN2KVI3key5valueLj2EE11update_nodeEPKS2_mmRS3_(%class.KV.1* %115, i64 %116, i64 %117, %class.KV.1* dereferenceable(16) %20)
  store %class.KV.1* %118, %class.KV.1** %21, align 8
  %119 = load %class.KV.0*, %class.KV.0** %7, align 8
  %120 = getelementptr inbounds %class.KV.0, %class.KV.0* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 1>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.1*, %class.KV.1** %21, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %122, %class.KV.1* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.1*, %class.KV.1** %12, align 8
  %127 = getelementptr inbounds %class.KV.1, %class.KV.1* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj2EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.1* sret %22, %class.KV.1* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.1*, %class.KV.1** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.1* @_ZN2KVI3key5valueLj2EE11update_nodeEPKS2_mmRS3_(%class.KV.1* %133, i64 %134, i64 %135, %class.KV.1* dereferenceable(16) %22)
  store %class.KV.1* %136, %class.KV.1** %23, align 8
  %137 = load %class.KV.0*, %class.KV.0** %7, align 8
  %138 = getelementptr inbounds %class.KV.0, %class.KV.0* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 1>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.1*, %class.KV.1** %23, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %140, %class.KV.1* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.1*
  store %class.KV.1* %150, %class.KV.1** %24, align 8
  %151 = load %class.KV.1*, %class.KV.1** %24, align 8
  %152 = bitcast %class.KV.1* %151 to i8*
  %153 = load %class.KV.1*, %class.KV.1** %12, align 8
  %154 = bitcast %class.KV.1* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.1*, %class.KV.1** %24, align 8
  %160 = getelementptr inbounds %class.KV.1, %class.KV.1* %159, i64 %158
  %161 = bitcast %class.KV.1* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.1*, %class.KV.1** %12, align 8
  %164 = getelementptr inbounds %class.KV.1, %class.KV.1* %163, i64 %162
  %165 = bitcast %class.KV.1* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.1*, %class.KV.1** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.1, %class.KV.1* %170, i64 %171
  %173 = bitcast %class.KV.1* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.1*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EPKS0_PKS1_(%class.KV.1* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.1*, %class.KV.1** %24, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %182, %class.KV.1* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.1* @_ZN2KVI3key5valueLj2EE11update_nodeEPKS2_mmRS3_(%class.KV.1*, i64, i64, %class.KV.1* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.1*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.1*, align 8
  %9 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.1* %3, %class.KV.1** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.1*
  store %class.KV.1* %13, %class.KV.1** %9, align 8
  %14 = load %class.KV.1*, %class.KV.1** %9, align 8
  %15 = bitcast %class.KV.1* %14 to i8*
  %16 = load %class.KV.1*, %class.KV.1** %5, align 8
  %17 = bitcast %class.KV.1* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.1*, %class.KV.1** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.1, %class.KV.1* %20, i64 %21
  %23 = bitcast %class.KV.1* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.1*
  %25 = load %class.KV.1*, %class.KV.1** %8, align 8
  call void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1* %24, %class.KV.1* dereferenceable(16) %25)
  %26 = load %class.KV.1*, %class.KV.1** %9, align 8
  ret %class.KV.1* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.1* noalias sret, %class.KV.1* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.1*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.2*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.2*, align 8
  %19 = alloca %class.KV.2, align 8
  %20 = alloca %class.KV.2, align 8
  %21 = alloca %class.KV.2*, align 8
  %22 = alloca %class.KV.2, align 8
  %23 = alloca %class.KV.2*, align 8
  %24 = alloca %class.KV.2*, align 8
  store %class.KV.1* %1, %class.KV.1** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.1*, %class.KV.1** %7, align 8
  %26 = getelementptr inbounds %class.KV.1, %class.KV.1* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 2>::Val"* %26 to %class.KV.2**
  %28 = load %class.KV.2*, %class.KV.2** %27, align 8
  store %class.KV.2* %28, %class.KV.2** %12, align 8
  %29 = load %class.KV.1*, %class.KV.1** %7, align 8
  %30 = getelementptr inbounds %class.KV.1, %class.KV.1* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 2>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.2*, %class.KV.2** %12, align 8
  %60 = getelementptr inbounds %class.KV.2, %class.KV.2* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.2, %class.KV.2* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 3>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.2*, %class.KV.2** %12, align 8
  %69 = getelementptr inbounds %class.KV.2, %class.KV.2* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.2, %class.KV.2* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 3>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.2*, %class.KV.2** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.2* @_ZN2KVI3key5valueLj3EE11update_nodeEPKS2_mmRS3_(%class.KV.2* %76, i64 %77, i64 %78, %class.KV.2* dereferenceable(16) %19)
  store %class.KV.2* %81, %class.KV.2** %18, align 8
  %82 = load %class.KV.1*, %class.KV.1** %7, align 8
  %83 = getelementptr inbounds %class.KV.1, %class.KV.1* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 2>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.2*, %class.KV.2** %18, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %85, %class.KV.2* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.2*, %class.KV.2** %12, align 8
  %93 = getelementptr inbounds %class.KV.2, %class.KV.2* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.2, %class.KV.2* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 3>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 22
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.2*, %class.KV.2** %12, align 8
  %101 = getelementptr inbounds %class.KV.2, %class.KV.2* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.2, %class.KV.2* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 3>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.2*, %class.KV.2** %12, align 8
  %107 = getelementptr inbounds %class.KV.2, %class.KV.2* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.2, %class.KV.2* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 3>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj3EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.2* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.2*, %class.KV.2** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.2* @_ZN2KVI3key5valueLj3EE11update_nodeEPKS2_mmRS3_(%class.KV.2* %115, i64 %116, i64 %117, %class.KV.2* dereferenceable(16) %20)
  store %class.KV.2* %118, %class.KV.2** %21, align 8
  %119 = load %class.KV.1*, %class.KV.1** %7, align 8
  %120 = getelementptr inbounds %class.KV.1, %class.KV.1* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 2>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.2*, %class.KV.2** %21, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %122, %class.KV.2* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.2*, %class.KV.2** %12, align 8
  %127 = getelementptr inbounds %class.KV.2, %class.KV.2* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj3EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.2* sret %22, %class.KV.2* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.2*, %class.KV.2** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.2* @_ZN2KVI3key5valueLj3EE11update_nodeEPKS2_mmRS3_(%class.KV.2* %133, i64 %134, i64 %135, %class.KV.2* dereferenceable(16) %22)
  store %class.KV.2* %136, %class.KV.2** %23, align 8
  %137 = load %class.KV.1*, %class.KV.1** %7, align 8
  %138 = getelementptr inbounds %class.KV.1, %class.KV.1* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 2>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.2*, %class.KV.2** %23, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %140, %class.KV.2* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.2*
  store %class.KV.2* %150, %class.KV.2** %24, align 8
  %151 = load %class.KV.2*, %class.KV.2** %24, align 8
  %152 = bitcast %class.KV.2* %151 to i8*
  %153 = load %class.KV.2*, %class.KV.2** %12, align 8
  %154 = bitcast %class.KV.2* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.2*, %class.KV.2** %24, align 8
  %160 = getelementptr inbounds %class.KV.2, %class.KV.2* %159, i64 %158
  %161 = bitcast %class.KV.2* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.2*, %class.KV.2** %12, align 8
  %164 = getelementptr inbounds %class.KV.2, %class.KV.2* %163, i64 %162
  %165 = bitcast %class.KV.2* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.2*, %class.KV.2** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.2, %class.KV.2* %170, i64 %171
  %173 = bitcast %class.KV.2* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.2*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EPKS0_PKS1_(%class.KV.2* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.2*, %class.KV.2** %24, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %182, %class.KV.2* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.2* @_ZN2KVI3key5valueLj3EE11update_nodeEPKS2_mmRS3_(%class.KV.2*, i64, i64, %class.KV.2* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.2*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.2*, align 8
  %9 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.2* %3, %class.KV.2** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.2*
  store %class.KV.2* %13, %class.KV.2** %9, align 8
  %14 = load %class.KV.2*, %class.KV.2** %9, align 8
  %15 = bitcast %class.KV.2* %14 to i8*
  %16 = load %class.KV.2*, %class.KV.2** %5, align 8
  %17 = bitcast %class.KV.2* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.2*, %class.KV.2** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.2, %class.KV.2* %20, i64 %21
  %23 = bitcast %class.KV.2* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.2*
  %25 = load %class.KV.2*, %class.KV.2** %8, align 8
  call void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2* %24, %class.KV.2* dereferenceable(16) %25)
  %26 = load %class.KV.2*, %class.KV.2** %9, align 8
  ret %class.KV.2* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.2* noalias sret, %class.KV.2* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.2*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.3*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.3*, align 8
  %19 = alloca %class.KV.3, align 8
  %20 = alloca %class.KV.3, align 8
  %21 = alloca %class.KV.3*, align 8
  %22 = alloca %class.KV.3, align 8
  %23 = alloca %class.KV.3*, align 8
  %24 = alloca %class.KV.3*, align 8
  store %class.KV.2* %1, %class.KV.2** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.2*, %class.KV.2** %7, align 8
  %26 = getelementptr inbounds %class.KV.2, %class.KV.2* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 3>::Val"* %26 to %class.KV.3**
  %28 = load %class.KV.3*, %class.KV.3** %27, align 8
  store %class.KV.3* %28, %class.KV.3** %12, align 8
  %29 = load %class.KV.2*, %class.KV.2** %7, align 8
  %30 = getelementptr inbounds %class.KV.2, %class.KV.2* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 3>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.3*, %class.KV.3** %12, align 8
  %60 = getelementptr inbounds %class.KV.3, %class.KV.3* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.3, %class.KV.3* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 4>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.3*, %class.KV.3** %12, align 8
  %69 = getelementptr inbounds %class.KV.3, %class.KV.3* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.3, %class.KV.3* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 4>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.3*, %class.KV.3** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.3* @_ZN2KVI3key5valueLj4EE11update_nodeEPKS2_mmRS3_(%class.KV.3* %76, i64 %77, i64 %78, %class.KV.3* dereferenceable(16) %19)
  store %class.KV.3* %81, %class.KV.3** %18, align 8
  %82 = load %class.KV.2*, %class.KV.2** %7, align 8
  %83 = getelementptr inbounds %class.KV.2, %class.KV.2* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 3>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.3*, %class.KV.3** %18, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %85, %class.KV.3* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.3*, %class.KV.3** %12, align 8
  %93 = getelementptr inbounds %class.KV.3, %class.KV.3* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.3, %class.KV.3* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 4>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 28
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.3*, %class.KV.3** %12, align 8
  %101 = getelementptr inbounds %class.KV.3, %class.KV.3* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.3, %class.KV.3* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 4>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.3*, %class.KV.3** %12, align 8
  %107 = getelementptr inbounds %class.KV.3, %class.KV.3* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.3, %class.KV.3* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 4>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj4EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.3* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.3*, %class.KV.3** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.3* @_ZN2KVI3key5valueLj4EE11update_nodeEPKS2_mmRS3_(%class.KV.3* %115, i64 %116, i64 %117, %class.KV.3* dereferenceable(16) %20)
  store %class.KV.3* %118, %class.KV.3** %21, align 8
  %119 = load %class.KV.2*, %class.KV.2** %7, align 8
  %120 = getelementptr inbounds %class.KV.2, %class.KV.2* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 3>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.3*, %class.KV.3** %21, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %122, %class.KV.3* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.3*, %class.KV.3** %12, align 8
  %127 = getelementptr inbounds %class.KV.3, %class.KV.3* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj4EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.3* sret %22, %class.KV.3* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.3*, %class.KV.3** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.3* @_ZN2KVI3key5valueLj4EE11update_nodeEPKS2_mmRS3_(%class.KV.3* %133, i64 %134, i64 %135, %class.KV.3* dereferenceable(16) %22)
  store %class.KV.3* %136, %class.KV.3** %23, align 8
  %137 = load %class.KV.2*, %class.KV.2** %7, align 8
  %138 = getelementptr inbounds %class.KV.2, %class.KV.2* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 3>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.3*, %class.KV.3** %23, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %140, %class.KV.3* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.3*
  store %class.KV.3* %150, %class.KV.3** %24, align 8
  %151 = load %class.KV.3*, %class.KV.3** %24, align 8
  %152 = bitcast %class.KV.3* %151 to i8*
  %153 = load %class.KV.3*, %class.KV.3** %12, align 8
  %154 = bitcast %class.KV.3* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.3*, %class.KV.3** %24, align 8
  %160 = getelementptr inbounds %class.KV.3, %class.KV.3* %159, i64 %158
  %161 = bitcast %class.KV.3* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.3*, %class.KV.3** %12, align 8
  %164 = getelementptr inbounds %class.KV.3, %class.KV.3* %163, i64 %162
  %165 = bitcast %class.KV.3* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.3*, %class.KV.3** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.3, %class.KV.3* %170, i64 %171
  %173 = bitcast %class.KV.3* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.3*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EPKS0_PKS1_(%class.KV.3* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.3*, %class.KV.3** %24, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %182, %class.KV.3* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.3* @_ZN2KVI3key5valueLj4EE11update_nodeEPKS2_mmRS3_(%class.KV.3*, i64, i64, %class.KV.3* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.3*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.3*, align 8
  %9 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.3* %3, %class.KV.3** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.3*
  store %class.KV.3* %13, %class.KV.3** %9, align 8
  %14 = load %class.KV.3*, %class.KV.3** %9, align 8
  %15 = bitcast %class.KV.3* %14 to i8*
  %16 = load %class.KV.3*, %class.KV.3** %5, align 8
  %17 = bitcast %class.KV.3* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.3*, %class.KV.3** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.3, %class.KV.3* %20, i64 %21
  %23 = bitcast %class.KV.3* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.3*
  %25 = load %class.KV.3*, %class.KV.3** %8, align 8
  call void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3* %24, %class.KV.3* dereferenceable(16) %25)
  %26 = load %class.KV.3*, %class.KV.3** %9, align 8
  ret %class.KV.3* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.3* noalias sret, %class.KV.3* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.3*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.4*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.4*, align 8
  %19 = alloca %class.KV.4, align 8
  %20 = alloca %class.KV.4, align 8
  %21 = alloca %class.KV.4*, align 8
  %22 = alloca %class.KV.4, align 8
  %23 = alloca %class.KV.4*, align 8
  %24 = alloca %class.KV.4*, align 8
  store %class.KV.3* %1, %class.KV.3** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.3*, %class.KV.3** %7, align 8
  %26 = getelementptr inbounds %class.KV.3, %class.KV.3* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 4>::Val"* %26 to %class.KV.4**
  %28 = load %class.KV.4*, %class.KV.4** %27, align 8
  store %class.KV.4* %28, %class.KV.4** %12, align 8
  %29 = load %class.KV.3*, %class.KV.3** %7, align 8
  %30 = getelementptr inbounds %class.KV.3, %class.KV.3* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 4>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.4*, %class.KV.4** %12, align 8
  %60 = getelementptr inbounds %class.KV.4, %class.KV.4* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.4, %class.KV.4* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 5>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.4*, %class.KV.4** %12, align 8
  %69 = getelementptr inbounds %class.KV.4, %class.KV.4* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.4, %class.KV.4* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 5>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.4*, %class.KV.4** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.4* @_ZN2KVI3key5valueLj5EE11update_nodeEPKS2_mmRS3_(%class.KV.4* %76, i64 %77, i64 %78, %class.KV.4* dereferenceable(16) %19)
  store %class.KV.4* %81, %class.KV.4** %18, align 8
  %82 = load %class.KV.3*, %class.KV.3** %7, align 8
  %83 = getelementptr inbounds %class.KV.3, %class.KV.3* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 4>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.4*, %class.KV.4** %18, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %85, %class.KV.4* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.4*, %class.KV.4** %12, align 8
  %93 = getelementptr inbounds %class.KV.4, %class.KV.4* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.4, %class.KV.4* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 5>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 34
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.4*, %class.KV.4** %12, align 8
  %101 = getelementptr inbounds %class.KV.4, %class.KV.4* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.4, %class.KV.4* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 5>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.4*, %class.KV.4** %12, align 8
  %107 = getelementptr inbounds %class.KV.4, %class.KV.4* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.4, %class.KV.4* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 5>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj5EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.4* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.4*, %class.KV.4** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.4* @_ZN2KVI3key5valueLj5EE11update_nodeEPKS2_mmRS3_(%class.KV.4* %115, i64 %116, i64 %117, %class.KV.4* dereferenceable(16) %20)
  store %class.KV.4* %118, %class.KV.4** %21, align 8
  %119 = load %class.KV.3*, %class.KV.3** %7, align 8
  %120 = getelementptr inbounds %class.KV.3, %class.KV.3* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 4>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.4*, %class.KV.4** %21, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %122, %class.KV.4* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.4*, %class.KV.4** %12, align 8
  %127 = getelementptr inbounds %class.KV.4, %class.KV.4* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj5EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.4* sret %22, %class.KV.4* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.4*, %class.KV.4** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.4* @_ZN2KVI3key5valueLj5EE11update_nodeEPKS2_mmRS3_(%class.KV.4* %133, i64 %134, i64 %135, %class.KV.4* dereferenceable(16) %22)
  store %class.KV.4* %136, %class.KV.4** %23, align 8
  %137 = load %class.KV.3*, %class.KV.3** %7, align 8
  %138 = getelementptr inbounds %class.KV.3, %class.KV.3* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 4>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.4*, %class.KV.4** %23, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %140, %class.KV.4* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.4*
  store %class.KV.4* %150, %class.KV.4** %24, align 8
  %151 = load %class.KV.4*, %class.KV.4** %24, align 8
  %152 = bitcast %class.KV.4* %151 to i8*
  %153 = load %class.KV.4*, %class.KV.4** %12, align 8
  %154 = bitcast %class.KV.4* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.4*, %class.KV.4** %24, align 8
  %160 = getelementptr inbounds %class.KV.4, %class.KV.4* %159, i64 %158
  %161 = bitcast %class.KV.4* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.4*, %class.KV.4** %12, align 8
  %164 = getelementptr inbounds %class.KV.4, %class.KV.4* %163, i64 %162
  %165 = bitcast %class.KV.4* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.4*, %class.KV.4** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.4, %class.KV.4* %170, i64 %171
  %173 = bitcast %class.KV.4* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.4*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EPKS0_PKS1_(%class.KV.4* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.4*, %class.KV.4** %24, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %182, %class.KV.4* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.4* @_ZN2KVI3key5valueLj5EE11update_nodeEPKS2_mmRS3_(%class.KV.4*, i64, i64, %class.KV.4* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.4*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.4*, align 8
  %9 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.4* %3, %class.KV.4** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.4*
  store %class.KV.4* %13, %class.KV.4** %9, align 8
  %14 = load %class.KV.4*, %class.KV.4** %9, align 8
  %15 = bitcast %class.KV.4* %14 to i8*
  %16 = load %class.KV.4*, %class.KV.4** %5, align 8
  %17 = bitcast %class.KV.4* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.4*, %class.KV.4** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.4, %class.KV.4* %20, i64 %21
  %23 = bitcast %class.KV.4* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.4*
  %25 = load %class.KV.4*, %class.KV.4** %8, align 8
  call void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4* %24, %class.KV.4* dereferenceable(16) %25)
  %26 = load %class.KV.4*, %class.KV.4** %9, align 8
  ret %class.KV.4* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.4* noalias sret, %class.KV.4* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.4*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.5*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.5*, align 8
  %19 = alloca %class.KV.5, align 8
  %20 = alloca %class.KV.5, align 8
  %21 = alloca %class.KV.5*, align 8
  %22 = alloca %class.KV.5, align 8
  %23 = alloca %class.KV.5*, align 8
  %24 = alloca %class.KV.5*, align 8
  store %class.KV.4* %1, %class.KV.4** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.4*, %class.KV.4** %7, align 8
  %26 = getelementptr inbounds %class.KV.4, %class.KV.4* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 5>::Val"* %26 to %class.KV.5**
  %28 = load %class.KV.5*, %class.KV.5** %27, align 8
  store %class.KV.5* %28, %class.KV.5** %12, align 8
  %29 = load %class.KV.4*, %class.KV.4** %7, align 8
  %30 = getelementptr inbounds %class.KV.4, %class.KV.4* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 5>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.5*, %class.KV.5** %12, align 8
  %60 = getelementptr inbounds %class.KV.5, %class.KV.5* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.5, %class.KV.5* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 6>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.5*, %class.KV.5** %12, align 8
  %69 = getelementptr inbounds %class.KV.5, %class.KV.5* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.5, %class.KV.5* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 6>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.5*, %class.KV.5** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.5* @_ZN2KVI3key5valueLj6EE11update_nodeEPKS2_mmRS3_(%class.KV.5* %76, i64 %77, i64 %78, %class.KV.5* dereferenceable(16) %19)
  store %class.KV.5* %81, %class.KV.5** %18, align 8
  %82 = load %class.KV.4*, %class.KV.4** %7, align 8
  %83 = getelementptr inbounds %class.KV.4, %class.KV.4* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 5>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.5*, %class.KV.5** %18, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %85, %class.KV.5* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.5*, %class.KV.5** %12, align 8
  %93 = getelementptr inbounds %class.KV.5, %class.KV.5* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.5, %class.KV.5* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 6>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 40
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.5*, %class.KV.5** %12, align 8
  %101 = getelementptr inbounds %class.KV.5, %class.KV.5* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.5, %class.KV.5* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 6>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.5*, %class.KV.5** %12, align 8
  %107 = getelementptr inbounds %class.KV.5, %class.KV.5* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.5, %class.KV.5* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 6>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj6EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.5* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.5*, %class.KV.5** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.5* @_ZN2KVI3key5valueLj6EE11update_nodeEPKS2_mmRS3_(%class.KV.5* %115, i64 %116, i64 %117, %class.KV.5* dereferenceable(16) %20)
  store %class.KV.5* %118, %class.KV.5** %21, align 8
  %119 = load %class.KV.4*, %class.KV.4** %7, align 8
  %120 = getelementptr inbounds %class.KV.4, %class.KV.4* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 5>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.5*, %class.KV.5** %21, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %122, %class.KV.5* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.5*, %class.KV.5** %12, align 8
  %127 = getelementptr inbounds %class.KV.5, %class.KV.5* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj6EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.5* sret %22, %class.KV.5* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.5*, %class.KV.5** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.5* @_ZN2KVI3key5valueLj6EE11update_nodeEPKS2_mmRS3_(%class.KV.5* %133, i64 %134, i64 %135, %class.KV.5* dereferenceable(16) %22)
  store %class.KV.5* %136, %class.KV.5** %23, align 8
  %137 = load %class.KV.4*, %class.KV.4** %7, align 8
  %138 = getelementptr inbounds %class.KV.4, %class.KV.4* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 5>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.5*, %class.KV.5** %23, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %140, %class.KV.5* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.5*
  store %class.KV.5* %150, %class.KV.5** %24, align 8
  %151 = load %class.KV.5*, %class.KV.5** %24, align 8
  %152 = bitcast %class.KV.5* %151 to i8*
  %153 = load %class.KV.5*, %class.KV.5** %12, align 8
  %154 = bitcast %class.KV.5* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.5*, %class.KV.5** %24, align 8
  %160 = getelementptr inbounds %class.KV.5, %class.KV.5* %159, i64 %158
  %161 = bitcast %class.KV.5* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.5*, %class.KV.5** %12, align 8
  %164 = getelementptr inbounds %class.KV.5, %class.KV.5* %163, i64 %162
  %165 = bitcast %class.KV.5* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.5*, %class.KV.5** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.5, %class.KV.5* %170, i64 %171
  %173 = bitcast %class.KV.5* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.5*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EPKS0_PKS1_(%class.KV.5* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.5*, %class.KV.5** %24, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %182, %class.KV.5* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.5* @_ZN2KVI3key5valueLj6EE11update_nodeEPKS2_mmRS3_(%class.KV.5*, i64, i64, %class.KV.5* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.5*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.5*, align 8
  %9 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.5* %3, %class.KV.5** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.5*
  store %class.KV.5* %13, %class.KV.5** %9, align 8
  %14 = load %class.KV.5*, %class.KV.5** %9, align 8
  %15 = bitcast %class.KV.5* %14 to i8*
  %16 = load %class.KV.5*, %class.KV.5** %5, align 8
  %17 = bitcast %class.KV.5* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.5*, %class.KV.5** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.5, %class.KV.5* %20, i64 %21
  %23 = bitcast %class.KV.5* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.5*
  %25 = load %class.KV.5*, %class.KV.5** %8, align 8
  call void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5* %24, %class.KV.5* dereferenceable(16) %25)
  %26 = load %class.KV.5*, %class.KV.5** %9, align 8
  ret %class.KV.5* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.5* noalias sret, %class.KV.5* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.5*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.6*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.6*, align 8
  %19 = alloca %class.KV.6, align 8
  %20 = alloca %class.KV.6, align 8
  %21 = alloca %class.KV.6*, align 8
  %22 = alloca %class.KV.6, align 8
  %23 = alloca %class.KV.6*, align 8
  %24 = alloca %class.KV.6*, align 8
  store %class.KV.5* %1, %class.KV.5** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.5*, %class.KV.5** %7, align 8
  %26 = getelementptr inbounds %class.KV.5, %class.KV.5* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 6>::Val"* %26 to %class.KV.6**
  %28 = load %class.KV.6*, %class.KV.6** %27, align 8
  store %class.KV.6* %28, %class.KV.6** %12, align 8
  %29 = load %class.KV.5*, %class.KV.5** %7, align 8
  %30 = getelementptr inbounds %class.KV.5, %class.KV.5* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 6>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.6*, %class.KV.6** %12, align 8
  %60 = getelementptr inbounds %class.KV.6, %class.KV.6* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.6, %class.KV.6* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 7>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.6*, %class.KV.6** %12, align 8
  %69 = getelementptr inbounds %class.KV.6, %class.KV.6* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.6, %class.KV.6* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 7>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.6*, %class.KV.6** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.6* @_ZN2KVI3key5valueLj7EE11update_nodeEPKS2_mmRS3_(%class.KV.6* %76, i64 %77, i64 %78, %class.KV.6* dereferenceable(16) %19)
  store %class.KV.6* %81, %class.KV.6** %18, align 8
  %82 = load %class.KV.5*, %class.KV.5** %7, align 8
  %83 = getelementptr inbounds %class.KV.5, %class.KV.5* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 6>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.6*, %class.KV.6** %18, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %85, %class.KV.6* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.6*, %class.KV.6** %12, align 8
  %93 = getelementptr inbounds %class.KV.6, %class.KV.6* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.6, %class.KV.6* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 7>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 46
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.6*, %class.KV.6** %12, align 8
  %101 = getelementptr inbounds %class.KV.6, %class.KV.6* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.6, %class.KV.6* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 7>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.6*, %class.KV.6** %12, align 8
  %107 = getelementptr inbounds %class.KV.6, %class.KV.6* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.6, %class.KV.6* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 7>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj7EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.6* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.6*, %class.KV.6** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.6* @_ZN2KVI3key5valueLj7EE11update_nodeEPKS2_mmRS3_(%class.KV.6* %115, i64 %116, i64 %117, %class.KV.6* dereferenceable(16) %20)
  store %class.KV.6* %118, %class.KV.6** %21, align 8
  %119 = load %class.KV.5*, %class.KV.5** %7, align 8
  %120 = getelementptr inbounds %class.KV.5, %class.KV.5* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 6>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.6*, %class.KV.6** %21, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %122, %class.KV.6* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.6*, %class.KV.6** %12, align 8
  %127 = getelementptr inbounds %class.KV.6, %class.KV.6* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj7EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.6* sret %22, %class.KV.6* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.6*, %class.KV.6** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.6* @_ZN2KVI3key5valueLj7EE11update_nodeEPKS2_mmRS3_(%class.KV.6* %133, i64 %134, i64 %135, %class.KV.6* dereferenceable(16) %22)
  store %class.KV.6* %136, %class.KV.6** %23, align 8
  %137 = load %class.KV.5*, %class.KV.5** %7, align 8
  %138 = getelementptr inbounds %class.KV.5, %class.KV.5* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 6>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.6*, %class.KV.6** %23, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %140, %class.KV.6* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.6*
  store %class.KV.6* %150, %class.KV.6** %24, align 8
  %151 = load %class.KV.6*, %class.KV.6** %24, align 8
  %152 = bitcast %class.KV.6* %151 to i8*
  %153 = load %class.KV.6*, %class.KV.6** %12, align 8
  %154 = bitcast %class.KV.6* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.6*, %class.KV.6** %24, align 8
  %160 = getelementptr inbounds %class.KV.6, %class.KV.6* %159, i64 %158
  %161 = bitcast %class.KV.6* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.6*, %class.KV.6** %12, align 8
  %164 = getelementptr inbounds %class.KV.6, %class.KV.6* %163, i64 %162
  %165 = bitcast %class.KV.6* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.6*, %class.KV.6** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.6, %class.KV.6* %170, i64 %171
  %173 = bitcast %class.KV.6* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.6*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EPKS0_PKS1_(%class.KV.6* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.6*, %class.KV.6** %24, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %182, %class.KV.6* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.6* @_ZN2KVI3key5valueLj7EE11update_nodeEPKS2_mmRS3_(%class.KV.6*, i64, i64, %class.KV.6* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.6*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.6*, align 8
  %9 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.6* %3, %class.KV.6** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.6*
  store %class.KV.6* %13, %class.KV.6** %9, align 8
  %14 = load %class.KV.6*, %class.KV.6** %9, align 8
  %15 = bitcast %class.KV.6* %14 to i8*
  %16 = load %class.KV.6*, %class.KV.6** %5, align 8
  %17 = bitcast %class.KV.6* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.6*, %class.KV.6** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.6, %class.KV.6* %20, i64 %21
  %23 = bitcast %class.KV.6* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.6*
  %25 = load %class.KV.6*, %class.KV.6** %8, align 8
  call void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6* %24, %class.KV.6* dereferenceable(16) %25)
  %26 = load %class.KV.6*, %class.KV.6** %9, align 8
  ret %class.KV.6* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.6* noalias sret, %class.KV.6* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.6*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.7*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.7*, align 8
  %19 = alloca %class.KV.7, align 8
  %20 = alloca %class.KV.7, align 8
  %21 = alloca %class.KV.7*, align 8
  %22 = alloca %class.KV.7, align 8
  %23 = alloca %class.KV.7*, align 8
  %24 = alloca %class.KV.7*, align 8
  store %class.KV.6* %1, %class.KV.6** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.6*, %class.KV.6** %7, align 8
  %26 = getelementptr inbounds %class.KV.6, %class.KV.6* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 7>::Val"* %26 to %class.KV.7**
  %28 = load %class.KV.7*, %class.KV.7** %27, align 8
  store %class.KV.7* %28, %class.KV.7** %12, align 8
  %29 = load %class.KV.6*, %class.KV.6** %7, align 8
  %30 = getelementptr inbounds %class.KV.6, %class.KV.6* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 7>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.7*, %class.KV.7** %12, align 8
  %60 = getelementptr inbounds %class.KV.7, %class.KV.7* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.7, %class.KV.7* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 8>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.7*, %class.KV.7** %12, align 8
  %69 = getelementptr inbounds %class.KV.7, %class.KV.7* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.7, %class.KV.7* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 8>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.7*, %class.KV.7** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.7* @_ZN2KVI3key5valueLj8EE11update_nodeEPKS2_mmRS3_(%class.KV.7* %76, i64 %77, i64 %78, %class.KV.7* dereferenceable(16) %19)
  store %class.KV.7* %81, %class.KV.7** %18, align 8
  %82 = load %class.KV.6*, %class.KV.6** %7, align 8
  %83 = getelementptr inbounds %class.KV.6, %class.KV.6* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 7>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.7*, %class.KV.7** %18, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %85, %class.KV.7* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.7*, %class.KV.7** %12, align 8
  %93 = getelementptr inbounds %class.KV.7, %class.KV.7* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.7, %class.KV.7* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 8>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 52
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.7*, %class.KV.7** %12, align 8
  %101 = getelementptr inbounds %class.KV.7, %class.KV.7* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.7, %class.KV.7* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 8>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.7*, %class.KV.7** %12, align 8
  %107 = getelementptr inbounds %class.KV.7, %class.KV.7* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.7, %class.KV.7* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 8>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj8EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.7* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.7*, %class.KV.7** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.7* @_ZN2KVI3key5valueLj8EE11update_nodeEPKS2_mmRS3_(%class.KV.7* %115, i64 %116, i64 %117, %class.KV.7* dereferenceable(16) %20)
  store %class.KV.7* %118, %class.KV.7** %21, align 8
  %119 = load %class.KV.6*, %class.KV.6** %7, align 8
  %120 = getelementptr inbounds %class.KV.6, %class.KV.6* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 7>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.7*, %class.KV.7** %21, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %122, %class.KV.7* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.7*, %class.KV.7** %12, align 8
  %127 = getelementptr inbounds %class.KV.7, %class.KV.7* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj8EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.7* sret %22, %class.KV.7* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.7*, %class.KV.7** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.7* @_ZN2KVI3key5valueLj8EE11update_nodeEPKS2_mmRS3_(%class.KV.7* %133, i64 %134, i64 %135, %class.KV.7* dereferenceable(16) %22)
  store %class.KV.7* %136, %class.KV.7** %23, align 8
  %137 = load %class.KV.6*, %class.KV.6** %7, align 8
  %138 = getelementptr inbounds %class.KV.6, %class.KV.6* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 7>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.7*, %class.KV.7** %23, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %140, %class.KV.7* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.7*
  store %class.KV.7* %150, %class.KV.7** %24, align 8
  %151 = load %class.KV.7*, %class.KV.7** %24, align 8
  %152 = bitcast %class.KV.7* %151 to i8*
  %153 = load %class.KV.7*, %class.KV.7** %12, align 8
  %154 = bitcast %class.KV.7* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.7*, %class.KV.7** %24, align 8
  %160 = getelementptr inbounds %class.KV.7, %class.KV.7* %159, i64 %158
  %161 = bitcast %class.KV.7* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.7*, %class.KV.7** %12, align 8
  %164 = getelementptr inbounds %class.KV.7, %class.KV.7* %163, i64 %162
  %165 = bitcast %class.KV.7* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.7*, %class.KV.7** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.7, %class.KV.7* %170, i64 %171
  %173 = bitcast %class.KV.7* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.7*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EPKS0_PKS1_(%class.KV.7* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.7*, %class.KV.7** %24, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %182, %class.KV.7* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.7* @_ZN2KVI3key5valueLj8EE11update_nodeEPKS2_mmRS3_(%class.KV.7*, i64, i64, %class.KV.7* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.7*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.7*, align 8
  %9 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.7* %3, %class.KV.7** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.7*
  store %class.KV.7* %13, %class.KV.7** %9, align 8
  %14 = load %class.KV.7*, %class.KV.7** %9, align 8
  %15 = bitcast %class.KV.7* %14 to i8*
  %16 = load %class.KV.7*, %class.KV.7** %5, align 8
  %17 = bitcast %class.KV.7* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.7*, %class.KV.7** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.7, %class.KV.7* %20, i64 %21
  %23 = bitcast %class.KV.7* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.7*
  %25 = load %class.KV.7*, %class.KV.7** %8, align 8
  call void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7* %24, %class.KV.7* dereferenceable(16) %25)
  %26 = load %class.KV.7*, %class.KV.7** %9, align 8
  ret %class.KV.7* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.7* noalias sret, %class.KV.7* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.7*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.8*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.8*, align 8
  %19 = alloca %class.KV.8, align 8
  %20 = alloca %class.KV.8, align 8
  %21 = alloca %class.KV.8*, align 8
  %22 = alloca %class.KV.8, align 8
  %23 = alloca %class.KV.8*, align 8
  %24 = alloca %class.KV.8*, align 8
  store %class.KV.7* %1, %class.KV.7** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.7*, %class.KV.7** %7, align 8
  %26 = getelementptr inbounds %class.KV.7, %class.KV.7* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 8>::Val"* %26 to %class.KV.8**
  %28 = load %class.KV.8*, %class.KV.8** %27, align 8
  store %class.KV.8* %28, %class.KV.8** %12, align 8
  %29 = load %class.KV.7*, %class.KV.7** %7, align 8
  %30 = getelementptr inbounds %class.KV.7, %class.KV.7* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 8>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.8*, %class.KV.8** %12, align 8
  %60 = getelementptr inbounds %class.KV.8, %class.KV.8* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.8, %class.KV.8* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 9>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.8*, %class.KV.8** %12, align 8
  %69 = getelementptr inbounds %class.KV.8, %class.KV.8* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.8, %class.KV.8* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 9>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.8*, %class.KV.8** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.8* @_ZN2KVI3key5valueLj9EE11update_nodeEPKS2_mmRS3_(%class.KV.8* %76, i64 %77, i64 %78, %class.KV.8* dereferenceable(16) %19)
  store %class.KV.8* %81, %class.KV.8** %18, align 8
  %82 = load %class.KV.7*, %class.KV.7** %7, align 8
  %83 = getelementptr inbounds %class.KV.7, %class.KV.7* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 8>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.8*, %class.KV.8** %18, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %85, %class.KV.8* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.8*, %class.KV.8** %12, align 8
  %93 = getelementptr inbounds %class.KV.8, %class.KV.8* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.8, %class.KV.8* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 9>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 58
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.8*, %class.KV.8** %12, align 8
  %101 = getelementptr inbounds %class.KV.8, %class.KV.8* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.8, %class.KV.8* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 9>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.8*, %class.KV.8** %12, align 8
  %107 = getelementptr inbounds %class.KV.8, %class.KV.8* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.8, %class.KV.8* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 9>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj9EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.8* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.8*, %class.KV.8** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.8* @_ZN2KVI3key5valueLj9EE11update_nodeEPKS2_mmRS3_(%class.KV.8* %115, i64 %116, i64 %117, %class.KV.8* dereferenceable(16) %20)
  store %class.KV.8* %118, %class.KV.8** %21, align 8
  %119 = load %class.KV.7*, %class.KV.7** %7, align 8
  %120 = getelementptr inbounds %class.KV.7, %class.KV.7* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 8>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.8*, %class.KV.8** %21, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %122, %class.KV.8* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.8*, %class.KV.8** %12, align 8
  %127 = getelementptr inbounds %class.KV.8, %class.KV.8* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj9EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.8* sret %22, %class.KV.8* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.8*, %class.KV.8** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.8* @_ZN2KVI3key5valueLj9EE11update_nodeEPKS2_mmRS3_(%class.KV.8* %133, i64 %134, i64 %135, %class.KV.8* dereferenceable(16) %22)
  store %class.KV.8* %136, %class.KV.8** %23, align 8
  %137 = load %class.KV.7*, %class.KV.7** %7, align 8
  %138 = getelementptr inbounds %class.KV.7, %class.KV.7* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 8>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.8*, %class.KV.8** %23, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %140, %class.KV.8* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.8*
  store %class.KV.8* %150, %class.KV.8** %24, align 8
  %151 = load %class.KV.8*, %class.KV.8** %24, align 8
  %152 = bitcast %class.KV.8* %151 to i8*
  %153 = load %class.KV.8*, %class.KV.8** %12, align 8
  %154 = bitcast %class.KV.8* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.8*, %class.KV.8** %24, align 8
  %160 = getelementptr inbounds %class.KV.8, %class.KV.8* %159, i64 %158
  %161 = bitcast %class.KV.8* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.8*, %class.KV.8** %12, align 8
  %164 = getelementptr inbounds %class.KV.8, %class.KV.8* %163, i64 %162
  %165 = bitcast %class.KV.8* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.8*, %class.KV.8** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.8, %class.KV.8* %170, i64 %171
  %173 = bitcast %class.KV.8* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.8*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EPKS0_PKS1_(%class.KV.8* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.8*, %class.KV.8** %24, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %182, %class.KV.8* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.8* @_ZN2KVI3key5valueLj9EE11update_nodeEPKS2_mmRS3_(%class.KV.8*, i64, i64, %class.KV.8* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.8*, align 8
  %9 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.8* %3, %class.KV.8** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.8*
  store %class.KV.8* %13, %class.KV.8** %9, align 8
  %14 = load %class.KV.8*, %class.KV.8** %9, align 8
  %15 = bitcast %class.KV.8* %14 to i8*
  %16 = load %class.KV.8*, %class.KV.8** %5, align 8
  %17 = bitcast %class.KV.8* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.8*, %class.KV.8** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.8, %class.KV.8* %20, i64 %21
  %23 = bitcast %class.KV.8* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.8*
  %25 = load %class.KV.8*, %class.KV.8** %8, align 8
  call void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8* %24, %class.KV.8* dereferenceable(16) %25)
  %26 = load %class.KV.8*, %class.KV.8** %9, align 8
  ret %class.KV.8* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.8* noalias sret, %class.KV.8* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.8*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.9*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i64, align 8
  %15 = alloca i64, align 8
  %16 = alloca i64, align 8
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.9*, align 8
  %19 = alloca %class.KV.9, align 8
  %20 = alloca %class.KV.9, align 8
  %21 = alloca %class.KV.9*, align 8
  %22 = alloca %class.KV.9, align 8
  %23 = alloca %class.KV.9*, align 8
  %24 = alloca %class.KV.9*, align 8
  store %class.KV.8* %1, %class.KV.8** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.8*, %class.KV.8** %7, align 8
  %26 = getelementptr inbounds %class.KV.8, %class.KV.8* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 9>::Val"* %26 to %class.KV.9**
  %28 = load %class.KV.9*, %class.KV.9** %27, align 8
  store %class.KV.9* %28, %class.KV.9** %12, align 8
  %29 = load %class.KV.8*, %class.KV.8** %7, align 8
  %30 = getelementptr inbounds %class.KV.8, %class.KV.8* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 9>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  store i64 %36, i64* %14, align 8
  %37 = load i64, i64* %13, align 8
  %38 = call i64 @llvm.ctpop.i64(i64 %37)
  %39 = trunc i64 %38 to i32
  %40 = sext i32 %39 to i64
  store i64 %40, i64* %15, align 8
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i64, i64* %14, align 8
  %44 = sub i64 63, %43
  %45 = shl i64 %42, %44
  %46 = call i64 @llvm.ctpop.i64(i64 %45)
  %47 = trunc i64 %46 to i32
  %48 = sext i32 %47 to i64
  store i64 %48, i64* %16, align 8
  %49 = load i64, i64* %13, align 8
  %50 = load i64, i64* %14, align 8
  %51 = shl i64 1, %50
  %52 = and i64 %49, %51
  %53 = icmp ne i64 %52, 0
  %54 = zext i1 %53 to i8
  store i8 %54, i8* %17, align 1
  %55 = load i8, i8* %17, align 1
  %56 = trunc i8 %55 to i1
  br i1 %56, label %57, label %142

; <label>:57                                      ; preds = %6
  %58 = load i64, i64* %16, align 8
  %59 = load %class.KV.9*, %class.KV.9** %12, align 8
  %60 = getelementptr inbounds %class.KV.9, %class.KV.9* %59, i64 %58
  %61 = getelementptr inbounds %class.KV.9, %class.KV.9* %60, i32 0, i32 0
  %62 = bitcast %"union.KV<key, value, 10>::Key"* %61 to i64*
  %63 = load i64, i64* %62, align 8
  %64 = and i64 %63, 1
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %66, label %124

; <label>:66                                      ; preds = %57
  %67 = load i64, i64* %16, align 8
  %68 = load %class.KV.9*, %class.KV.9** %12, align 8
  %69 = getelementptr inbounds %class.KV.9, %class.KV.9* %68, i64 %67
  %70 = getelementptr inbounds %class.KV.9, %class.KV.9* %69, i32 0, i32 0
  %71 = bitcast %"union.KV<key, value, 10>::Key"* %70 to %class.key**
  %72 = load %class.key*, %class.key** %71, align 8
  %73 = load %class.key*, %class.key** %9, align 8
  %74 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %72, %class.key* dereferenceable(8) %73)
  br i1 %74, label %75, label %87

; <label>:75                                      ; preds = %66
  %76 = load %class.KV.9*, %class.KV.9** %12, align 8
  %77 = load i64, i64* %15, align 8
  %78 = load i64, i64* %16, align 8
  %79 = load %class.key*, %class.key** %9, align 8
  %80 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %19, %class.key* %79, %class.value* %80)
  %81 = call %class.KV.9* @_ZN2KVI3key5valueLj10EE11update_nodeEPKS2_mmRS3_(%class.KV.9* %76, i64 %77, i64 %78, %class.KV.9* dereferenceable(16) %19)
  store %class.KV.9* %81, %class.KV.9** %18, align 8
  %82 = load %class.KV.8*, %class.KV.8** %7, align 8
  %83 = getelementptr inbounds %class.KV.8, %class.KV.8* %82, i32 0, i32 0
  %84 = bitcast %"union.KV<key, value, 9>::Key"* %83 to i64*
  %85 = load i64, i64* %84, align 8
  %86 = load %class.KV.9*, %class.KV.9** %18, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %85, %class.KV.9* %86)
  br label %184

; <label>:87                                      ; preds = %66
  %88 = load i64*, i64** %11, align 8
  %89 = load i64, i64* %88, align 8
  %90 = add i64 %89, 1
  store i64 %90, i64* %88, align 8
  %91 = load i64, i64* %16, align 8
  %92 = load %class.KV.9*, %class.KV.9** %12, align 8
  %93 = getelementptr inbounds %class.KV.9, %class.KV.9* %92, i64 %91
  %94 = getelementptr inbounds %class.KV.9, %class.KV.9* %93, i32 0, i32 0
  %95 = bitcast %"union.KV<key, value, 10>::Key"* %94 to %class.key**
  %96 = load %class.key*, %class.key** %95, align 8
  %97 = call i64 @_ZNK3key4hashEv(%class.key* %96)
  %98 = lshr i64 %97, 0
  %99 = load i64, i64* %16, align 8
  %100 = load %class.KV.9*, %class.KV.9** %12, align 8
  %101 = getelementptr inbounds %class.KV.9, %class.KV.9* %100, i64 %99
  %102 = getelementptr inbounds %class.KV.9, %class.KV.9* %101, i32 0, i32 0
  %103 = bitcast %"union.KV<key, value, 10>::Key"* %102 to %class.key**
  %104 = load %class.key*, %class.key** %103, align 8
  %105 = load i64, i64* %16, align 8
  %106 = load %class.KV.9*, %class.KV.9** %12, align 8
  %107 = getelementptr inbounds %class.KV.9, %class.KV.9* %106, i64 %105
  %108 = getelementptr inbounds %class.KV.9, %class.KV.9* %107, i32 0, i32 1
  %109 = bitcast %"union.KV<key, value, 10>::Val"* %108 to %class.value**
  %110 = load %class.value*, %class.value** %109, align 8
  %111 = load i64, i64* %8, align 8
  %112 = lshr i64 %111, 6
  %113 = load %class.key*, %class.key** %9, align 8
  %114 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EE14new_inner_nodeEyPKS0_PKS1_yS4_S6_(%class.KV.9* sret %20, i64 %98, %class.key* %104, %class.value* %110, i64 %112, %class.key* %113, %class.value* %114)
  %115 = load %class.KV.9*, %class.KV.9** %12, align 8
  %116 = load i64, i64* %15, align 8
  %117 = load i64, i64* %16, align 8
  %118 = call %class.KV.9* @_ZN2KVI3key5valueLj10EE11update_nodeEPKS2_mmRS3_(%class.KV.9* %115, i64 %116, i64 %117, %class.KV.9* dereferenceable(16) %20)
  store %class.KV.9* %118, %class.KV.9** %21, align 8
  %119 = load %class.KV.8*, %class.KV.8** %7, align 8
  %120 = getelementptr inbounds %class.KV.8, %class.KV.8* %119, i32 0, i32 0
  %121 = bitcast %"union.KV<key, value, 9>::Key"* %120 to i64*
  %122 = load i64, i64* %121, align 8
  %123 = load %class.KV.9*, %class.KV.9** %21, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %122, %class.KV.9* %123)
  br label %184

; <label>:124                                     ; preds = %57
  %125 = load i64, i64* %16, align 8
  %126 = load %class.KV.9*, %class.KV.9** %12, align 8
  %127 = getelementptr inbounds %class.KV.9, %class.KV.9* %126, i64 %125
  %128 = load i64, i64* %8, align 8
  %129 = lshr i64 %128, 6
  %130 = load %class.key*, %class.key** %9, align 8
  %131 = load %class.value*, %class.value** %10, align 8
  %132 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3key5valueLj10EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.9* sret %22, %class.KV.9* dereferenceable(16) %127, i64 %129, %class.key* %130, %class.value* %131, i64* %132)
  %133 = load %class.KV.9*, %class.KV.9** %12, align 8
  %134 = load i64, i64* %15, align 8
  %135 = load i64, i64* %16, align 8
  %136 = call %class.KV.9* @_ZN2KVI3key5valueLj10EE11update_nodeEPKS2_mmRS3_(%class.KV.9* %133, i64 %134, i64 %135, %class.KV.9* dereferenceable(16) %22)
  store %class.KV.9* %136, %class.KV.9** %23, align 8
  %137 = load %class.KV.8*, %class.KV.8** %7, align 8
  %138 = getelementptr inbounds %class.KV.8, %class.KV.8* %137, i32 0, i32 0
  %139 = bitcast %"union.KV<key, value, 9>::Key"* %138 to i64*
  %140 = load i64, i64* %139, align 8
  %141 = load %class.KV.9*, %class.KV.9** %23, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %140, %class.KV.9* %141)
  br label %184

; <label>:142                                     ; preds = %6
  %143 = load i64*, i64** %11, align 8
  %144 = load i64, i64* %143, align 8
  %145 = add i64 %144, 1
  store i64 %145, i64* %143, align 8
  %146 = load i64, i64* %15, align 8
  %147 = add i64 %146, 1
  %148 = mul i64 %147, 16
  %149 = call i8* @malloc(i64 %148)
  %150 = bitcast i8* %149 to %class.KV.9*
  store %class.KV.9* %150, %class.KV.9** %24, align 8
  %151 = load %class.KV.9*, %class.KV.9** %24, align 8
  %152 = bitcast %class.KV.9* %151 to i8*
  %153 = load %class.KV.9*, %class.KV.9** %12, align 8
  %154 = bitcast %class.KV.9* %153 to i8*
  %155 = load i64, i64* %16, align 8
  %156 = mul i64 %155, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %152, i8* %154, i64 %156, i32 8, i1 false)
  %157 = load i64, i64* %16, align 8
  %158 = add i64 %157, 1
  %159 = load %class.KV.9*, %class.KV.9** %24, align 8
  %160 = getelementptr inbounds %class.KV.9, %class.KV.9* %159, i64 %158
  %161 = bitcast %class.KV.9* %160 to i8*
  %162 = load i64, i64* %16, align 8
  %163 = load %class.KV.9*, %class.KV.9** %12, align 8
  %164 = getelementptr inbounds %class.KV.9, %class.KV.9* %163, i64 %162
  %165 = bitcast %class.KV.9* %164 to i8*
  %166 = load i64, i64* %15, align 8
  %167 = load i64, i64* %16, align 8
  %168 = sub i64 %166, %167
  %169 = mul i64 %168, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %161, i8* %165, i64 %169, i32 8, i1 false)
  %170 = load %class.KV.9*, %class.KV.9** %24, align 8
  %171 = load i64, i64* %16, align 8
  %172 = getelementptr inbounds %class.KV.9, %class.KV.9* %170, i64 %171
  %173 = bitcast %class.KV.9* %172 to i8*
  %174 = bitcast i8* %173 to %class.KV.9*
  %175 = load %class.key*, %class.key** %9, align 8
  %176 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %174, %class.key* %175, %class.value* %176)
  %177 = load i64, i64* %13, align 8
  %178 = load i64, i64* %14, align 8
  %179 = shl i64 1, %178
  %180 = or i64 %177, %179
  %181 = shl i64 %180, 1
  %182 = or i64 %181, 1
  %183 = load %class.KV.9*, %class.KV.9** %24, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %182, %class.KV.9* %183)
  br label %184

; <label>:184                                     ; preds = %142, %124, %87, %75
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.KV.9* @_ZN2KVI3key5valueLj10EE11update_nodeEPKS2_mmRS3_(%class.KV.9*, i64, i64, %class.KV.9* dereferenceable(16)) #0 align 2 {
  %5 = alloca %class.KV.9*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.KV.9*, align 8
  %9 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %5, align 8
  store i64 %1, i64* %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.KV.9* %3, %class.KV.9** %8, align 8
  %10 = load i64, i64* %6, align 8
  %11 = mul i64 %10, 16
  %12 = call i8* @malloc(i64 %11)
  %13 = bitcast i8* %12 to %class.KV.9*
  store %class.KV.9* %13, %class.KV.9** %9, align 8
  %14 = load %class.KV.9*, %class.KV.9** %9, align 8
  %15 = bitcast %class.KV.9* %14 to i8*
  %16 = load %class.KV.9*, %class.KV.9** %5, align 8
  %17 = bitcast %class.KV.9* %16 to i8*
  %18 = load i64, i64* %6, align 8
  %19 = mul i64 %18, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %15, i8* %17, i64 %19, i32 8, i1 false)
  %20 = load %class.KV.9*, %class.KV.9** %9, align 8
  %21 = load i64, i64* %7, align 8
  %22 = getelementptr inbounds %class.KV.9, %class.KV.9* %20, i64 %21
  %23 = bitcast %class.KV.9* %22 to i8*
  %24 = bitcast i8* %23 to %class.KV.9*
  %25 = load %class.KV.9*, %class.KV.9** %8, align 8
  call void @_ZN2KVI3key5valueLj10EEC1ERKS2_(%class.KV.9* %24, %class.KV.9* dereferenceable(16) %25)
  %26 = load %class.KV.9*, %class.KV.9** %9, align 8
  ret %class.KV.9* %26
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE12insert_innerERKS2_yPKS0_PKS1_Py(%class.KV.9* noalias sret, %class.KV.9* dereferenceable(16), i64, %class.key*, %class.value*, i64*) #0 align 2 {
  %7 = alloca %class.KV.9*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.value*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.LL*, align 8
  %13 = alloca %class.LL*, align 8
  %14 = alloca %class.LL*, align 8
  store %class.KV.9* %1, %class.KV.9** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.value* %4, %class.value** %10, align 8
  store i64* %5, i64** %11, align 8
  %15 = load %class.KV.9*, %class.KV.9** %7, align 8
  %16 = getelementptr inbounds %class.KV.9, %class.KV.9* %15, i32 0, i32 0
  %17 = bitcast %"union.KV<key, value, 10>::Key"* %16 to i64*
  %18 = load i64, i64* %17, align 8
  %19 = and i64 %18, 1
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %58

; <label>:21                                      ; preds = %6
  %22 = load %class.KV.9*, %class.KV.9** %7, align 8
  %23 = getelementptr inbounds %class.KV.9, %class.KV.9* %22, i32 0, i32 0
  %24 = bitcast %"union.KV<key, value, 10>::Key"* %23 to %class.key**
  %25 = load %class.key*, %class.key** %24, align 8
  %26 = load %class.key*, %class.key** %9, align 8
  %27 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %25, %class.key* dereferenceable(8) %26)
  br i1 %27, label %28, label %34

; <label>:28                                      ; preds = %21
  %29 = load %class.KV.9*, %class.KV.9** %7, align 8
  %30 = getelementptr inbounds %class.KV.9, %class.KV.9* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, value, 10>::Key"* %30 to %class.key**
  %32 = load %class.key*, %class.key** %31, align 8
  %33 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EPKS0_PKS1_(%class.KV.9* %0, %class.key* %32, %class.value* %33)
  br label %84

; <label>:34                                      ; preds = %21
  %35 = load i64*, i64** %11, align 8
  %36 = load i64, i64* %35, align 8
  %37 = add i64 %36, 1
  store i64 %37, i64* %35, align 8
  %38 = call i8* @malloc(i64 24)
  %39 = bitcast i8* %38 to %class.LL*
  %40 = bitcast %class.LL* %39 to i8*
  %41 = bitcast i8* %40 to %class.LL*
  %42 = load %class.key*, %class.key** %9, align 8
  %43 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %41, %class.key* %42, %class.value* %43, %class.LL* null)
  store %class.LL* %41, %class.LL** %12, align 8
  %44 = call i8* @malloc(i64 24)
  %45 = bitcast i8* %44 to %class.LL*
  %46 = bitcast %class.LL* %45 to i8*
  %47 = bitcast i8* %46 to %class.LL*
  %48 = load %class.KV.9*, %class.KV.9** %7, align 8
  %49 = getelementptr inbounds %class.KV.9, %class.KV.9* %48, i32 0, i32 0
  %50 = bitcast %"union.KV<key, value, 10>::Key"* %49 to %class.key**
  %51 = load %class.key*, %class.key** %50, align 8
  %52 = load %class.KV.9*, %class.KV.9** %7, align 8
  %53 = getelementptr inbounds %class.KV.9, %class.KV.9* %52, i32 0, i32 1
  %54 = bitcast %"union.KV<key, value, 10>::Val"* %53 to %class.value**
  %55 = load %class.value*, %class.value** %54, align 8
  %56 = load %class.LL*, %class.LL** %12, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %47, %class.key* %51, %class.value* %55, %class.LL* %56)
  store %class.LL* %47, %class.LL** %13, align 8
  %57 = load %class.LL*, %class.LL** %13, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9* %0, i64 1, %class.LL* %57)
  br label %84

; <label>:58                                      ; preds = %6
  %59 = load %class.KV.9*, %class.KV.9** %7, align 8
  %60 = getelementptr inbounds %class.KV.9, %class.KV.9* %59, i32 0, i32 1
  %61 = bitcast %"union.KV<key, value, 10>::Val"* %60 to %class.LL**
  %62 = load %class.LL*, %class.LL** %61, align 8
  %63 = icmp ne %class.LL* %62, null
  br i1 %63, label %64, label %73

; <label>:64                                      ; preds = %58
  %65 = load %class.KV.9*, %class.KV.9** %7, align 8
  %66 = getelementptr inbounds %class.KV.9, %class.KV.9* %65, i32 0, i32 1
  %67 = bitcast %"union.KV<key, value, 10>::Val"* %66 to %class.LL**
  %68 = load %class.LL*, %class.LL** %67, align 8
  %69 = load %class.key*, %class.key** %9, align 8
  %70 = load %class.value*, %class.value** %10, align 8
  %71 = load i64*, i64** %11, align 8
  %72 = call %class.LL* @_ZNK2LLI3key5valueE6insertEPKS0_PKS1_Py(%class.LL* %68, %class.key* %69, %class.value* %70, i64* %71)
  call void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9* %0, i64 1, %class.LL* %72)
  br label %84

; <label>:73                                      ; preds = %58
  %74 = load i64*, i64** %11, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, 1
  store i64 %76, i64* %74, align 8
  %77 = call i8* @malloc(i64 24)
  %78 = bitcast i8* %77 to %class.LL*
  %79 = bitcast %class.LL* %78 to i8*
  %80 = bitcast i8* %79 to %class.LL*
  %81 = load %class.key*, %class.key** %9, align 8
  %82 = load %class.value*, %class.value** %10, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %80, %class.key* %81, %class.value* %82, %class.LL* null)
  store %class.LL* %80, %class.LL** %14, align 8
  %83 = load %class.LL*, %class.LL** %14, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9* %0, i64 1, %class.LL* %83)
  br label %84

; <label>:84                                      ; preds = %73, %64, %34, %28
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.LL* @_ZNK2LLI3key5valueE6insertEPKS0_PKS1_Py(%class.LL*, %class.key*, %class.value*, i64*) #0 align 2 {
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.LL*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.value*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.LL*, align 8
  %11 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %6, align 8
  store %class.key* %1, %class.key** %7, align 8
  store %class.value* %2, %class.value** %8, align 8
  store i64* %3, i64** %9, align 8
  %12 = load %class.LL*, %class.LL** %6, align 8
  %13 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %14 = load %class.key*, %class.key** %13, align 8
  %15 = load %class.key*, %class.key** %7, align 8
  %16 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %14, %class.key* dereferenceable(8) %15)
  br i1 %16, label %17, label %27

; <label>:17                                      ; preds = %4
  %18 = call i8* @malloc(i64 24)
  %19 = bitcast i8* %18 to %class.LL*
  %20 = bitcast %class.LL* %19 to i8*
  %21 = bitcast i8* %20 to %class.LL*
  %22 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %23 = load %class.key*, %class.key** %22, align 8
  %24 = load %class.value*, %class.value** %8, align 8
  %25 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %26 = load %class.LL*, %class.LL** %25, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %21, %class.key* %23, %class.value* %24, %class.LL* %26)
  store %class.LL* %21, %class.LL** %5, align 8
  br label %66

; <label>:27                                      ; preds = %4
  %28 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %29 = load %class.LL*, %class.LL** %28, align 8
  %30 = icmp ne %class.LL* %29, null
  br i1 %30, label %31, label %46

; <label>:31                                      ; preds = %27
  %32 = call i8* @malloc(i64 24)
  %33 = bitcast i8* %32 to %class.LL*
  %34 = bitcast %class.LL* %33 to i8*
  %35 = bitcast i8* %34 to %class.LL*
  %36 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %37 = load %class.key*, %class.key** %36, align 8
  %38 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 1
  %39 = load %class.value*, %class.value** %38, align 8
  %40 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %41 = load %class.LL*, %class.LL** %40, align 8
  %42 = load %class.key*, %class.key** %7, align 8
  %43 = load %class.value*, %class.value** %8, align 8
  %44 = load i64*, i64** %9, align 8
  %45 = call %class.LL* @_ZNK2LLI3key5valueE6insertEPKS0_PKS1_Py(%class.LL* %41, %class.key* %42, %class.value* %43, i64* %44)
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %35, %class.key* %37, %class.value* %39, %class.LL* %45)
  store %class.LL* %35, %class.LL** %5, align 8
  br label %66

; <label>:46                                      ; preds = %27
  %47 = load i64*, i64** %9, align 8
  %48 = load i64, i64* %47, align 8
  %49 = add i64 %48, 1
  store i64 %49, i64* %47, align 8
  %50 = call i8* @malloc(i64 24)
  %51 = bitcast i8* %50 to %class.LL*
  %52 = bitcast %class.LL* %51 to i8*
  %53 = bitcast i8* %52 to %class.LL*
  %54 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %55 = load %class.key*, %class.key** %54, align 8
  %56 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 1
  %57 = load %class.value*, %class.value** %56, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %53, %class.key* %55, %class.value* %57, %class.LL* null)
  store %class.LL* %53, %class.LL** %10, align 8
  %58 = call i8* @malloc(i64 24)
  %59 = bitcast i8* %58 to %class.LL*
  %60 = bitcast %class.LL* %59 to i8*
  %61 = bitcast i8* %60 to %class.LL*
  %62 = load %class.key*, %class.key** %7, align 8
  %63 = load %class.value*, %class.value** %8, align 8
  %64 = load %class.LL*, %class.LL** %10, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %61, %class.key* %62, %class.value* %63, %class.LL* %64)
  store %class.LL* %61, %class.LL** %11, align 8
  %65 = load %class.LL*, %class.LL** %11, align 8
  store %class.LL* %65, %class.LL** %5, align 8
  br label %66

; <label>:66                                      ; preds = %46, %31, %17
  %67 = load %class.LL*, %class.LL** %5, align 8
  ret %class.LL* %67
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EE12remove_innerERKS2_yPKS0_Py(%class.KV* noalias sret, %class.KV* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.0*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.0*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.0, align 8
  %19 = alloca %class.KV.0*, align 8
  store %class.KV* %1, %class.KV** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV*, %class.KV** %6, align 8
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 0>::Val"* %21 to %class.KV.0**
  %23 = load %class.KV.0*, %class.KV.0** %22, align 8
  store %class.KV.0* %23, %class.KV.0** %10, align 8
  %24 = load %class.KV*, %class.KV** %6, align 8
  %25 = getelementptr inbounds %class.KV, %class.KV* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 0>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.0*, %class.KV.0** %10, align 8
  %55 = getelementptr inbounds %class.KV.0, %class.KV.0* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.0, %class.KV.0* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 1>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.0*, %class.KV.0** %10, align 8
  %64 = getelementptr inbounds %class.KV.0, %class.KV.0* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.0, %class.KV.0* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 1>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.0*
  store %class.KV.0* %78, %class.KV.0** %16, align 8
  %79 = load %class.KV.0*, %class.KV.0** %16, align 8
  %80 = bitcast %class.KV.0* %79 to i8*
  %81 = load %class.KV.0*, %class.KV.0** %10, align 8
  %82 = bitcast %class.KV.0* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.0*, %class.KV.0** %16, align 8
  %87 = getelementptr inbounds %class.KV.0, %class.KV.0* %86, i64 %85
  %88 = bitcast %class.KV.0* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.0*, %class.KV.0** %10, align 8
  %92 = getelementptr inbounds %class.KV.0, %class.KV.0* %91, i64 %90
  %93 = bitcast %class.KV.0* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.0*, %class.KV.0** %16, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %106, %class.KV.0* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC1ERKS2_(%class.KV* %0, %class.KV* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.0*, %class.KV.0** %10, align 8
  %113 = getelementptr inbounds %class.KV.0, %class.KV.0* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj1EE12remove_innerERKS2_yPKS0_Py(%class.KV.0* sret %18, %class.KV.0* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.0*, %class.KV.0** %10, align 8
  %120 = getelementptr inbounds %class.KV.0, %class.KV.0* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj1EEeqERKS2_(%class.KV.0* %18, %class.KV.0* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC1ERKS2_(%class.KV* %0, %class.KV* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.0*, %class.KV.0** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.0* @_ZN2KVI3key5valueLj1EE11update_nodeEPKS2_mmRS3_(%class.KV.0* %125, i64 %126, i64 %127, %class.KV.0* dereferenceable(16) %18)
  store %class.KV.0* %128, %class.KV.0** %19, align 8
  %129 = load %class.KV*, %class.KV** %6, align 8
  %130 = getelementptr inbounds %class.KV, %class.KV* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 0>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.0*, %class.KV.0** %19, align 8
  call void @_ZN2KVI3key5valueLj0EEC1EyPKS_IS0_S1_Lj1EE(%class.KV* %0, i64 %132, %class.KV.0* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3key5valueLj0EEC1ERKS2_(%class.KV* %0, %class.KV* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj0EEeqERKS2_(%class.KV*, %class.KV* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV*, align 8
  %4 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %3, align 8
  store %class.KV* %1, %class.KV** %4, align 8
  %5 = load %class.KV*, %class.KV** %3, align 8
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 0>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV*, %class.KV** %4, align 8
  %10 = getelementptr inbounds %class.KV, %class.KV* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 0>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 0>::Val"* %15 to %class.KV.0**
  %17 = load %class.KV.0*, %class.KV.0** %16, align 8
  %18 = load %class.KV*, %class.KV** %4, align 8
  %19 = getelementptr inbounds %class.KV, %class.KV* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 0>::Val"* %19 to %class.KV.0**
  %21 = load %class.KV.0*, %class.KV.0** %20, align 8
  %22 = icmp eq %class.KV.0* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC1ERKS2_(%class.KV*, %class.KV* dereferenceable(16)) unnamed_addr #0 align 2 {
  %3 = alloca %class.KV*, align 8
  %4 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %3, align 8
  store %class.KV* %1, %class.KV** %4, align 8
  %5 = load %class.KV*, %class.KV** %3, align 8
  %6 = load %class.KV*, %class.KV** %4, align 8
  call void @_ZN2KVI3key5valueLj0EEC2ERKS2_(%class.KV* %5, %class.KV* dereferenceable(16) %6)
  ret void
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj1EE12remove_innerERKS2_yPKS0_Py(%class.KV.0* noalias sret, %class.KV.0* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.0*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.1*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.1*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.1, align 8
  %19 = alloca %class.KV.1*, align 8
  store %class.KV.0* %1, %class.KV.0** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.0*, %class.KV.0** %6, align 8
  %21 = getelementptr inbounds %class.KV.0, %class.KV.0* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 1>::Val"* %21 to %class.KV.1**
  %23 = load %class.KV.1*, %class.KV.1** %22, align 8
  store %class.KV.1* %23, %class.KV.1** %10, align 8
  %24 = load %class.KV.0*, %class.KV.0** %6, align 8
  %25 = getelementptr inbounds %class.KV.0, %class.KV.0* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 1>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.1*, %class.KV.1** %10, align 8
  %55 = getelementptr inbounds %class.KV.1, %class.KV.1* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.1, %class.KV.1* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 2>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.1*, %class.KV.1** %10, align 8
  %64 = getelementptr inbounds %class.KV.1, %class.KV.1* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.1, %class.KV.1* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 2>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.1*
  store %class.KV.1* %78, %class.KV.1** %16, align 8
  %79 = load %class.KV.1*, %class.KV.1** %16, align 8
  %80 = bitcast %class.KV.1* %79 to i8*
  %81 = load %class.KV.1*, %class.KV.1** %10, align 8
  %82 = bitcast %class.KV.1* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.1*, %class.KV.1** %16, align 8
  %87 = getelementptr inbounds %class.KV.1, %class.KV.1* %86, i64 %85
  %88 = bitcast %class.KV.1* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.1*, %class.KV.1** %10, align 8
  %92 = getelementptr inbounds %class.KV.1, %class.KV.1* %91, i64 %90
  %93 = bitcast %class.KV.1* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.1*, %class.KV.1** %16, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %106, %class.KV.1* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.1*, %class.KV.1** %10, align 8
  %113 = getelementptr inbounds %class.KV.1, %class.KV.1* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj2EE12remove_innerERKS2_yPKS0_Py(%class.KV.1* sret %18, %class.KV.1* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.1*, %class.KV.1** %10, align 8
  %120 = getelementptr inbounds %class.KV.1, %class.KV.1* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj2EEeqERKS2_(%class.KV.1* %18, %class.KV.1* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.1*, %class.KV.1** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.1* @_ZN2KVI3key5valueLj2EE11update_nodeEPKS2_mmRS3_(%class.KV.1* %125, i64 %126, i64 %127, %class.KV.1* dereferenceable(16) %18)
  store %class.KV.1* %128, %class.KV.1** %19, align 8
  %129 = load %class.KV.0*, %class.KV.0** %6, align 8
  %130 = getelementptr inbounds %class.KV.0, %class.KV.0* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 1>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.1*, %class.KV.1** %19, align 8
  call void @_ZN2KVI3key5valueLj1EEC1EyPKS_IS0_S1_Lj2EE(%class.KV.0* %0, i64 %132, %class.KV.1* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3key5valueLj1EEC1ERKS2_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj1EEeqERKS2_(%class.KV.0*, %class.KV.0* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.0*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %class.KV.0*, %class.KV.0** %3, align 8
  %6 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 1>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.0*, %class.KV.0** %4, align 8
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 1>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 1>::Val"* %15 to %class.KV.1**
  %17 = load %class.KV.1*, %class.KV.1** %16, align 8
  %18 = load %class.KV.0*, %class.KV.0** %4, align 8
  %19 = getelementptr inbounds %class.KV.0, %class.KV.0* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 1>::Val"* %19 to %class.KV.1**
  %21 = load %class.KV.1*, %class.KV.1** %20, align 8
  %22 = icmp eq %class.KV.1* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj2EE12remove_innerERKS2_yPKS0_Py(%class.KV.1* noalias sret, %class.KV.1* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.1*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.2*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.2*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.2, align 8
  %19 = alloca %class.KV.2*, align 8
  store %class.KV.1* %1, %class.KV.1** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.1*, %class.KV.1** %6, align 8
  %21 = getelementptr inbounds %class.KV.1, %class.KV.1* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 2>::Val"* %21 to %class.KV.2**
  %23 = load %class.KV.2*, %class.KV.2** %22, align 8
  store %class.KV.2* %23, %class.KV.2** %10, align 8
  %24 = load %class.KV.1*, %class.KV.1** %6, align 8
  %25 = getelementptr inbounds %class.KV.1, %class.KV.1* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 2>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.2*, %class.KV.2** %10, align 8
  %55 = getelementptr inbounds %class.KV.2, %class.KV.2* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.2, %class.KV.2* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 3>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.2*, %class.KV.2** %10, align 8
  %64 = getelementptr inbounds %class.KV.2, %class.KV.2* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.2, %class.KV.2* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 3>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.2*
  store %class.KV.2* %78, %class.KV.2** %16, align 8
  %79 = load %class.KV.2*, %class.KV.2** %16, align 8
  %80 = bitcast %class.KV.2* %79 to i8*
  %81 = load %class.KV.2*, %class.KV.2** %10, align 8
  %82 = bitcast %class.KV.2* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.2*, %class.KV.2** %16, align 8
  %87 = getelementptr inbounds %class.KV.2, %class.KV.2* %86, i64 %85
  %88 = bitcast %class.KV.2* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.2*, %class.KV.2** %10, align 8
  %92 = getelementptr inbounds %class.KV.2, %class.KV.2* %91, i64 %90
  %93 = bitcast %class.KV.2* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.2*, %class.KV.2** %16, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %106, %class.KV.2* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.2*, %class.KV.2** %10, align 8
  %113 = getelementptr inbounds %class.KV.2, %class.KV.2* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj3EE12remove_innerERKS2_yPKS0_Py(%class.KV.2* sret %18, %class.KV.2* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.2*, %class.KV.2** %10, align 8
  %120 = getelementptr inbounds %class.KV.2, %class.KV.2* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj3EEeqERKS2_(%class.KV.2* %18, %class.KV.2* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.2*, %class.KV.2** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.2* @_ZN2KVI3key5valueLj3EE11update_nodeEPKS2_mmRS3_(%class.KV.2* %125, i64 %126, i64 %127, %class.KV.2* dereferenceable(16) %18)
  store %class.KV.2* %128, %class.KV.2** %19, align 8
  %129 = load %class.KV.1*, %class.KV.1** %6, align 8
  %130 = getelementptr inbounds %class.KV.1, %class.KV.1* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 2>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.2*, %class.KV.2** %19, align 8
  call void @_ZN2KVI3key5valueLj2EEC1EyPKS_IS0_S1_Lj3EE(%class.KV.1* %0, i64 %132, %class.KV.2* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3key5valueLj2EEC1ERKS2_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj2EEeqERKS2_(%class.KV.1*, %class.KV.1* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.1*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %class.KV.1*, %class.KV.1** %3, align 8
  %6 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 2>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.1*, %class.KV.1** %4, align 8
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 2>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 2>::Val"* %15 to %class.KV.2**
  %17 = load %class.KV.2*, %class.KV.2** %16, align 8
  %18 = load %class.KV.1*, %class.KV.1** %4, align 8
  %19 = getelementptr inbounds %class.KV.1, %class.KV.1* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 2>::Val"* %19 to %class.KV.2**
  %21 = load %class.KV.2*, %class.KV.2** %20, align 8
  %22 = icmp eq %class.KV.2* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj3EE12remove_innerERKS2_yPKS0_Py(%class.KV.2* noalias sret, %class.KV.2* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.2*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.3*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.3*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.3, align 8
  %19 = alloca %class.KV.3*, align 8
  store %class.KV.2* %1, %class.KV.2** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.2*, %class.KV.2** %6, align 8
  %21 = getelementptr inbounds %class.KV.2, %class.KV.2* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 3>::Val"* %21 to %class.KV.3**
  %23 = load %class.KV.3*, %class.KV.3** %22, align 8
  store %class.KV.3* %23, %class.KV.3** %10, align 8
  %24 = load %class.KV.2*, %class.KV.2** %6, align 8
  %25 = getelementptr inbounds %class.KV.2, %class.KV.2* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 3>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.3*, %class.KV.3** %10, align 8
  %55 = getelementptr inbounds %class.KV.3, %class.KV.3* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.3, %class.KV.3* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 4>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.3*, %class.KV.3** %10, align 8
  %64 = getelementptr inbounds %class.KV.3, %class.KV.3* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.3, %class.KV.3* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 4>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.3*
  store %class.KV.3* %78, %class.KV.3** %16, align 8
  %79 = load %class.KV.3*, %class.KV.3** %16, align 8
  %80 = bitcast %class.KV.3* %79 to i8*
  %81 = load %class.KV.3*, %class.KV.3** %10, align 8
  %82 = bitcast %class.KV.3* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.3*, %class.KV.3** %16, align 8
  %87 = getelementptr inbounds %class.KV.3, %class.KV.3* %86, i64 %85
  %88 = bitcast %class.KV.3* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.3*, %class.KV.3** %10, align 8
  %92 = getelementptr inbounds %class.KV.3, %class.KV.3* %91, i64 %90
  %93 = bitcast %class.KV.3* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.3*, %class.KV.3** %16, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %106, %class.KV.3* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.3*, %class.KV.3** %10, align 8
  %113 = getelementptr inbounds %class.KV.3, %class.KV.3* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj4EE12remove_innerERKS2_yPKS0_Py(%class.KV.3* sret %18, %class.KV.3* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.3*, %class.KV.3** %10, align 8
  %120 = getelementptr inbounds %class.KV.3, %class.KV.3* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj4EEeqERKS2_(%class.KV.3* %18, %class.KV.3* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.3*, %class.KV.3** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.3* @_ZN2KVI3key5valueLj4EE11update_nodeEPKS2_mmRS3_(%class.KV.3* %125, i64 %126, i64 %127, %class.KV.3* dereferenceable(16) %18)
  store %class.KV.3* %128, %class.KV.3** %19, align 8
  %129 = load %class.KV.2*, %class.KV.2** %6, align 8
  %130 = getelementptr inbounds %class.KV.2, %class.KV.2* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 3>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.3*, %class.KV.3** %19, align 8
  call void @_ZN2KVI3key5valueLj3EEC1EyPKS_IS0_S1_Lj4EE(%class.KV.2* %0, i64 %132, %class.KV.3* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3key5valueLj3EEC1ERKS2_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj3EEeqERKS2_(%class.KV.2*, %class.KV.2* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.2*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %class.KV.2*, %class.KV.2** %3, align 8
  %6 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 3>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.2*, %class.KV.2** %4, align 8
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 3>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 3>::Val"* %15 to %class.KV.3**
  %17 = load %class.KV.3*, %class.KV.3** %16, align 8
  %18 = load %class.KV.2*, %class.KV.2** %4, align 8
  %19 = getelementptr inbounds %class.KV.2, %class.KV.2* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 3>::Val"* %19 to %class.KV.3**
  %21 = load %class.KV.3*, %class.KV.3** %20, align 8
  %22 = icmp eq %class.KV.3* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj4EE12remove_innerERKS2_yPKS0_Py(%class.KV.3* noalias sret, %class.KV.3* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.3*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.4*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.4*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.4, align 8
  %19 = alloca %class.KV.4*, align 8
  store %class.KV.3* %1, %class.KV.3** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.3*, %class.KV.3** %6, align 8
  %21 = getelementptr inbounds %class.KV.3, %class.KV.3* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 4>::Val"* %21 to %class.KV.4**
  %23 = load %class.KV.4*, %class.KV.4** %22, align 8
  store %class.KV.4* %23, %class.KV.4** %10, align 8
  %24 = load %class.KV.3*, %class.KV.3** %6, align 8
  %25 = getelementptr inbounds %class.KV.3, %class.KV.3* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 4>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.4*, %class.KV.4** %10, align 8
  %55 = getelementptr inbounds %class.KV.4, %class.KV.4* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.4, %class.KV.4* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 5>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.4*, %class.KV.4** %10, align 8
  %64 = getelementptr inbounds %class.KV.4, %class.KV.4* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.4, %class.KV.4* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 5>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.4*
  store %class.KV.4* %78, %class.KV.4** %16, align 8
  %79 = load %class.KV.4*, %class.KV.4** %16, align 8
  %80 = bitcast %class.KV.4* %79 to i8*
  %81 = load %class.KV.4*, %class.KV.4** %10, align 8
  %82 = bitcast %class.KV.4* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.4*, %class.KV.4** %16, align 8
  %87 = getelementptr inbounds %class.KV.4, %class.KV.4* %86, i64 %85
  %88 = bitcast %class.KV.4* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.4*, %class.KV.4** %10, align 8
  %92 = getelementptr inbounds %class.KV.4, %class.KV.4* %91, i64 %90
  %93 = bitcast %class.KV.4* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.4*, %class.KV.4** %16, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %106, %class.KV.4* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.4*, %class.KV.4** %10, align 8
  %113 = getelementptr inbounds %class.KV.4, %class.KV.4* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj5EE12remove_innerERKS2_yPKS0_Py(%class.KV.4* sret %18, %class.KV.4* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.4*, %class.KV.4** %10, align 8
  %120 = getelementptr inbounds %class.KV.4, %class.KV.4* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj5EEeqERKS2_(%class.KV.4* %18, %class.KV.4* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.4*, %class.KV.4** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.4* @_ZN2KVI3key5valueLj5EE11update_nodeEPKS2_mmRS3_(%class.KV.4* %125, i64 %126, i64 %127, %class.KV.4* dereferenceable(16) %18)
  store %class.KV.4* %128, %class.KV.4** %19, align 8
  %129 = load %class.KV.3*, %class.KV.3** %6, align 8
  %130 = getelementptr inbounds %class.KV.3, %class.KV.3* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 4>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.4*, %class.KV.4** %19, align 8
  call void @_ZN2KVI3key5valueLj4EEC1EyPKS_IS0_S1_Lj5EE(%class.KV.3* %0, i64 %132, %class.KV.4* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3key5valueLj4EEC1ERKS2_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj4EEeqERKS2_(%class.KV.3*, %class.KV.3* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.3*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %class.KV.3*, %class.KV.3** %3, align 8
  %6 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 4>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.3*, %class.KV.3** %4, align 8
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 4>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 4>::Val"* %15 to %class.KV.4**
  %17 = load %class.KV.4*, %class.KV.4** %16, align 8
  %18 = load %class.KV.3*, %class.KV.3** %4, align 8
  %19 = getelementptr inbounds %class.KV.3, %class.KV.3* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 4>::Val"* %19 to %class.KV.4**
  %21 = load %class.KV.4*, %class.KV.4** %20, align 8
  %22 = icmp eq %class.KV.4* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj5EE12remove_innerERKS2_yPKS0_Py(%class.KV.4* noalias sret, %class.KV.4* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.4*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.5*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.5*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.5, align 8
  %19 = alloca %class.KV.5*, align 8
  store %class.KV.4* %1, %class.KV.4** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.4*, %class.KV.4** %6, align 8
  %21 = getelementptr inbounds %class.KV.4, %class.KV.4* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 5>::Val"* %21 to %class.KV.5**
  %23 = load %class.KV.5*, %class.KV.5** %22, align 8
  store %class.KV.5* %23, %class.KV.5** %10, align 8
  %24 = load %class.KV.4*, %class.KV.4** %6, align 8
  %25 = getelementptr inbounds %class.KV.4, %class.KV.4* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 5>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.5*, %class.KV.5** %10, align 8
  %55 = getelementptr inbounds %class.KV.5, %class.KV.5* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.5, %class.KV.5* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 6>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.5*, %class.KV.5** %10, align 8
  %64 = getelementptr inbounds %class.KV.5, %class.KV.5* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.5, %class.KV.5* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 6>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.5*
  store %class.KV.5* %78, %class.KV.5** %16, align 8
  %79 = load %class.KV.5*, %class.KV.5** %16, align 8
  %80 = bitcast %class.KV.5* %79 to i8*
  %81 = load %class.KV.5*, %class.KV.5** %10, align 8
  %82 = bitcast %class.KV.5* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.5*, %class.KV.5** %16, align 8
  %87 = getelementptr inbounds %class.KV.5, %class.KV.5* %86, i64 %85
  %88 = bitcast %class.KV.5* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.5*, %class.KV.5** %10, align 8
  %92 = getelementptr inbounds %class.KV.5, %class.KV.5* %91, i64 %90
  %93 = bitcast %class.KV.5* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.5*, %class.KV.5** %16, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %106, %class.KV.5* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.5*, %class.KV.5** %10, align 8
  %113 = getelementptr inbounds %class.KV.5, %class.KV.5* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj6EE12remove_innerERKS2_yPKS0_Py(%class.KV.5* sret %18, %class.KV.5* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.5*, %class.KV.5** %10, align 8
  %120 = getelementptr inbounds %class.KV.5, %class.KV.5* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj6EEeqERKS2_(%class.KV.5* %18, %class.KV.5* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.5*, %class.KV.5** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.5* @_ZN2KVI3key5valueLj6EE11update_nodeEPKS2_mmRS3_(%class.KV.5* %125, i64 %126, i64 %127, %class.KV.5* dereferenceable(16) %18)
  store %class.KV.5* %128, %class.KV.5** %19, align 8
  %129 = load %class.KV.4*, %class.KV.4** %6, align 8
  %130 = getelementptr inbounds %class.KV.4, %class.KV.4* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 5>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.5*, %class.KV.5** %19, align 8
  call void @_ZN2KVI3key5valueLj5EEC1EyPKS_IS0_S1_Lj6EE(%class.KV.4* %0, i64 %132, %class.KV.5* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3key5valueLj5EEC1ERKS2_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj5EEeqERKS2_(%class.KV.4*, %class.KV.4* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.4*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %class.KV.4*, %class.KV.4** %3, align 8
  %6 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 5>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.4*, %class.KV.4** %4, align 8
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 5>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 5>::Val"* %15 to %class.KV.5**
  %17 = load %class.KV.5*, %class.KV.5** %16, align 8
  %18 = load %class.KV.4*, %class.KV.4** %4, align 8
  %19 = getelementptr inbounds %class.KV.4, %class.KV.4* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 5>::Val"* %19 to %class.KV.5**
  %21 = load %class.KV.5*, %class.KV.5** %20, align 8
  %22 = icmp eq %class.KV.5* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj6EE12remove_innerERKS2_yPKS0_Py(%class.KV.5* noalias sret, %class.KV.5* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.5*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.6*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.6*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.6, align 8
  %19 = alloca %class.KV.6*, align 8
  store %class.KV.5* %1, %class.KV.5** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.5*, %class.KV.5** %6, align 8
  %21 = getelementptr inbounds %class.KV.5, %class.KV.5* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 6>::Val"* %21 to %class.KV.6**
  %23 = load %class.KV.6*, %class.KV.6** %22, align 8
  store %class.KV.6* %23, %class.KV.6** %10, align 8
  %24 = load %class.KV.5*, %class.KV.5** %6, align 8
  %25 = getelementptr inbounds %class.KV.5, %class.KV.5* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 6>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.6*, %class.KV.6** %10, align 8
  %55 = getelementptr inbounds %class.KV.6, %class.KV.6* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.6, %class.KV.6* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 7>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.6*, %class.KV.6** %10, align 8
  %64 = getelementptr inbounds %class.KV.6, %class.KV.6* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.6, %class.KV.6* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 7>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.6*
  store %class.KV.6* %78, %class.KV.6** %16, align 8
  %79 = load %class.KV.6*, %class.KV.6** %16, align 8
  %80 = bitcast %class.KV.6* %79 to i8*
  %81 = load %class.KV.6*, %class.KV.6** %10, align 8
  %82 = bitcast %class.KV.6* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.6*, %class.KV.6** %16, align 8
  %87 = getelementptr inbounds %class.KV.6, %class.KV.6* %86, i64 %85
  %88 = bitcast %class.KV.6* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.6*, %class.KV.6** %10, align 8
  %92 = getelementptr inbounds %class.KV.6, %class.KV.6* %91, i64 %90
  %93 = bitcast %class.KV.6* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.6*, %class.KV.6** %16, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %106, %class.KV.6* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.6*, %class.KV.6** %10, align 8
  %113 = getelementptr inbounds %class.KV.6, %class.KV.6* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj7EE12remove_innerERKS2_yPKS0_Py(%class.KV.6* sret %18, %class.KV.6* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.6*, %class.KV.6** %10, align 8
  %120 = getelementptr inbounds %class.KV.6, %class.KV.6* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj7EEeqERKS2_(%class.KV.6* %18, %class.KV.6* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.6*, %class.KV.6** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.6* @_ZN2KVI3key5valueLj7EE11update_nodeEPKS2_mmRS3_(%class.KV.6* %125, i64 %126, i64 %127, %class.KV.6* dereferenceable(16) %18)
  store %class.KV.6* %128, %class.KV.6** %19, align 8
  %129 = load %class.KV.5*, %class.KV.5** %6, align 8
  %130 = getelementptr inbounds %class.KV.5, %class.KV.5* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 6>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.6*, %class.KV.6** %19, align 8
  call void @_ZN2KVI3key5valueLj6EEC1EyPKS_IS0_S1_Lj7EE(%class.KV.5* %0, i64 %132, %class.KV.6* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3key5valueLj6EEC1ERKS2_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj6EEeqERKS2_(%class.KV.5*, %class.KV.5* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.5*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %class.KV.5*, %class.KV.5** %3, align 8
  %6 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 6>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.5*, %class.KV.5** %4, align 8
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 6>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 6>::Val"* %15 to %class.KV.6**
  %17 = load %class.KV.6*, %class.KV.6** %16, align 8
  %18 = load %class.KV.5*, %class.KV.5** %4, align 8
  %19 = getelementptr inbounds %class.KV.5, %class.KV.5* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 6>::Val"* %19 to %class.KV.6**
  %21 = load %class.KV.6*, %class.KV.6** %20, align 8
  %22 = icmp eq %class.KV.6* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj7EE12remove_innerERKS2_yPKS0_Py(%class.KV.6* noalias sret, %class.KV.6* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.6*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.7*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.7*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.7, align 8
  %19 = alloca %class.KV.7*, align 8
  store %class.KV.6* %1, %class.KV.6** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.6*, %class.KV.6** %6, align 8
  %21 = getelementptr inbounds %class.KV.6, %class.KV.6* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 7>::Val"* %21 to %class.KV.7**
  %23 = load %class.KV.7*, %class.KV.7** %22, align 8
  store %class.KV.7* %23, %class.KV.7** %10, align 8
  %24 = load %class.KV.6*, %class.KV.6** %6, align 8
  %25 = getelementptr inbounds %class.KV.6, %class.KV.6* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 7>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.7*, %class.KV.7** %10, align 8
  %55 = getelementptr inbounds %class.KV.7, %class.KV.7* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.7, %class.KV.7* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 8>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.7*, %class.KV.7** %10, align 8
  %64 = getelementptr inbounds %class.KV.7, %class.KV.7* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.7, %class.KV.7* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 8>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.7*
  store %class.KV.7* %78, %class.KV.7** %16, align 8
  %79 = load %class.KV.7*, %class.KV.7** %16, align 8
  %80 = bitcast %class.KV.7* %79 to i8*
  %81 = load %class.KV.7*, %class.KV.7** %10, align 8
  %82 = bitcast %class.KV.7* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.7*, %class.KV.7** %16, align 8
  %87 = getelementptr inbounds %class.KV.7, %class.KV.7* %86, i64 %85
  %88 = bitcast %class.KV.7* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.7*, %class.KV.7** %10, align 8
  %92 = getelementptr inbounds %class.KV.7, %class.KV.7* %91, i64 %90
  %93 = bitcast %class.KV.7* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.7*, %class.KV.7** %16, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %106, %class.KV.7* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.7*, %class.KV.7** %10, align 8
  %113 = getelementptr inbounds %class.KV.7, %class.KV.7* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj8EE12remove_innerERKS2_yPKS0_Py(%class.KV.7* sret %18, %class.KV.7* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.7*, %class.KV.7** %10, align 8
  %120 = getelementptr inbounds %class.KV.7, %class.KV.7* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj8EEeqERKS2_(%class.KV.7* %18, %class.KV.7* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.7*, %class.KV.7** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.7* @_ZN2KVI3key5valueLj8EE11update_nodeEPKS2_mmRS3_(%class.KV.7* %125, i64 %126, i64 %127, %class.KV.7* dereferenceable(16) %18)
  store %class.KV.7* %128, %class.KV.7** %19, align 8
  %129 = load %class.KV.6*, %class.KV.6** %6, align 8
  %130 = getelementptr inbounds %class.KV.6, %class.KV.6* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 7>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.7*, %class.KV.7** %19, align 8
  call void @_ZN2KVI3key5valueLj7EEC1EyPKS_IS0_S1_Lj8EE(%class.KV.6* %0, i64 %132, %class.KV.7* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3key5valueLj7EEC1ERKS2_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj7EEeqERKS2_(%class.KV.6*, %class.KV.6* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.6*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %class.KV.6*, %class.KV.6** %3, align 8
  %6 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 7>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.6*, %class.KV.6** %4, align 8
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 7>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 7>::Val"* %15 to %class.KV.7**
  %17 = load %class.KV.7*, %class.KV.7** %16, align 8
  %18 = load %class.KV.6*, %class.KV.6** %4, align 8
  %19 = getelementptr inbounds %class.KV.6, %class.KV.6* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 7>::Val"* %19 to %class.KV.7**
  %21 = load %class.KV.7*, %class.KV.7** %20, align 8
  %22 = icmp eq %class.KV.7* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj8EE12remove_innerERKS2_yPKS0_Py(%class.KV.7* noalias sret, %class.KV.7* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.7*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.8*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.8*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.8, align 8
  %19 = alloca %class.KV.8*, align 8
  store %class.KV.7* %1, %class.KV.7** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.7*, %class.KV.7** %6, align 8
  %21 = getelementptr inbounds %class.KV.7, %class.KV.7* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 8>::Val"* %21 to %class.KV.8**
  %23 = load %class.KV.8*, %class.KV.8** %22, align 8
  store %class.KV.8* %23, %class.KV.8** %10, align 8
  %24 = load %class.KV.7*, %class.KV.7** %6, align 8
  %25 = getelementptr inbounds %class.KV.7, %class.KV.7* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 8>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.8*, %class.KV.8** %10, align 8
  %55 = getelementptr inbounds %class.KV.8, %class.KV.8* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.8, %class.KV.8* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 9>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.8*, %class.KV.8** %10, align 8
  %64 = getelementptr inbounds %class.KV.8, %class.KV.8* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.8, %class.KV.8* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 9>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.8*
  store %class.KV.8* %78, %class.KV.8** %16, align 8
  %79 = load %class.KV.8*, %class.KV.8** %16, align 8
  %80 = bitcast %class.KV.8* %79 to i8*
  %81 = load %class.KV.8*, %class.KV.8** %10, align 8
  %82 = bitcast %class.KV.8* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.8*, %class.KV.8** %16, align 8
  %87 = getelementptr inbounds %class.KV.8, %class.KV.8* %86, i64 %85
  %88 = bitcast %class.KV.8* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.8*, %class.KV.8** %10, align 8
  %92 = getelementptr inbounds %class.KV.8, %class.KV.8* %91, i64 %90
  %93 = bitcast %class.KV.8* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.8*, %class.KV.8** %16, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %106, %class.KV.8* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.8*, %class.KV.8** %10, align 8
  %113 = getelementptr inbounds %class.KV.8, %class.KV.8* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj9EE12remove_innerERKS2_yPKS0_Py(%class.KV.8* sret %18, %class.KV.8* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.8*, %class.KV.8** %10, align 8
  %120 = getelementptr inbounds %class.KV.8, %class.KV.8* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj9EEeqERKS2_(%class.KV.8* %18, %class.KV.8* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.8*, %class.KV.8** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.8* @_ZN2KVI3key5valueLj9EE11update_nodeEPKS2_mmRS3_(%class.KV.8* %125, i64 %126, i64 %127, %class.KV.8* dereferenceable(16) %18)
  store %class.KV.8* %128, %class.KV.8** %19, align 8
  %129 = load %class.KV.7*, %class.KV.7** %6, align 8
  %130 = getelementptr inbounds %class.KV.7, %class.KV.7* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 8>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.8*, %class.KV.8** %19, align 8
  call void @_ZN2KVI3key5valueLj8EEC1EyPKS_IS0_S1_Lj9EE(%class.KV.7* %0, i64 %132, %class.KV.8* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3key5valueLj8EEC1ERKS2_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj8EEeqERKS2_(%class.KV.7*, %class.KV.7* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.7*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %class.KV.7*, %class.KV.7** %3, align 8
  %6 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 8>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.7*, %class.KV.7** %4, align 8
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 8>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 8>::Val"* %15 to %class.KV.8**
  %17 = load %class.KV.8*, %class.KV.8** %16, align 8
  %18 = load %class.KV.7*, %class.KV.7** %4, align 8
  %19 = getelementptr inbounds %class.KV.7, %class.KV.7* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 8>::Val"* %19 to %class.KV.8**
  %21 = load %class.KV.8*, %class.KV.8** %20, align 8
  %22 = icmp eq %class.KV.8* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj9EE12remove_innerERKS2_yPKS0_Py(%class.KV.8* noalias sret, %class.KV.8* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.8*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.9*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i64, align 8
  %14 = alloca i8, align 1
  %15 = alloca i64, align 8
  %16 = alloca %class.KV.9*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.9, align 8
  %19 = alloca %class.KV.9*, align 8
  store %class.KV.8* %1, %class.KV.8** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.8*, %class.KV.8** %6, align 8
  %21 = getelementptr inbounds %class.KV.8, %class.KV.8* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, value, 9>::Val"* %21 to %class.KV.9**
  %23 = load %class.KV.9*, %class.KV.9** %22, align 8
  store %class.KV.9* %23, %class.KV.9** %10, align 8
  %24 = load %class.KV.8*, %class.KV.8** %6, align 8
  %25 = getelementptr inbounds %class.KV.8, %class.KV.8* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, value, 9>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  store i64 %31, i64* %12, align 8
  %32 = load i64, i64* %11, align 8
  %33 = call i64 @llvm.ctpop.i64(i64 %32)
  %34 = trunc i64 %33 to i32
  %35 = sext i32 %34 to i64
  store i64 %35, i64* %13, align 8
  %36 = load i64, i64* %11, align 8
  %37 = load i64, i64* %12, align 8
  %38 = shl i64 1, %37
  %39 = and i64 %36, %38
  %40 = icmp ne i64 %39, 0
  %41 = zext i1 %40 to i8
  store i8 %41, i8* %14, align 1
  %42 = load i8, i8* %14, align 1
  %43 = trunc i8 %42 to i1
  br i1 %43, label %44, label %134

; <label>:44                                      ; preds = %5
  %45 = load i64, i64* %11, align 8
  %46 = shl i64 %45, 1
  %47 = load i64, i64* %12, align 8
  %48 = sub i64 63, %47
  %49 = shl i64 %46, %48
  %50 = call i64 @llvm.ctpop.i64(i64 %49)
  %51 = trunc i64 %50 to i32
  %52 = sext i32 %51 to i64
  store i64 %52, i64* %15, align 8
  %53 = load i64, i64* %15, align 8
  %54 = load %class.KV.9*, %class.KV.9** %10, align 8
  %55 = getelementptr inbounds %class.KV.9, %class.KV.9* %54, i64 %53
  %56 = getelementptr inbounds %class.KV.9, %class.KV.9* %55, i32 0, i32 0
  %57 = bitcast %"union.KV<key, value, 10>::Key"* %56 to i64*
  %58 = load i64, i64* %57, align 8
  %59 = and i64 %58, 1
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %61, label %110

; <label>:61                                      ; preds = %44
  %62 = load i64, i64* %15, align 8
  %63 = load %class.KV.9*, %class.KV.9** %10, align 8
  %64 = getelementptr inbounds %class.KV.9, %class.KV.9* %63, i64 %62
  %65 = getelementptr inbounds %class.KV.9, %class.KV.9* %64, i32 0, i32 0
  %66 = bitcast %"union.KV<key, value, 10>::Key"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  %68 = load %class.key*, %class.key** %8, align 8
  %69 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %67, %class.key* dereferenceable(8) %68)
  br i1 %69, label %70, label %108

; <label>:70                                      ; preds = %61
  %71 = load i64*, i64** %9, align 8
  %72 = load i64, i64* %71, align 8
  %73 = add i64 %72, -1
  store i64 %73, i64* %71, align 8
  %74 = load i64, i64* %13, align 8
  %75 = sub i64 %74, 1
  %76 = mul i64 %75, 16
  %77 = call i8* @malloc(i64 %76)
  %78 = bitcast i8* %77 to %class.KV.9*
  store %class.KV.9* %78, %class.KV.9** %16, align 8
  %79 = load %class.KV.9*, %class.KV.9** %16, align 8
  %80 = bitcast %class.KV.9* %79 to i8*
  %81 = load %class.KV.9*, %class.KV.9** %10, align 8
  %82 = bitcast %class.KV.9* %81 to i8*
  %83 = load i64, i64* %15, align 8
  %84 = mul i64 %83, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %80, i8* %82, i64 %84, i32 8, i1 false)
  %85 = load i64, i64* %15, align 8
  %86 = load %class.KV.9*, %class.KV.9** %16, align 8
  %87 = getelementptr inbounds %class.KV.9, %class.KV.9* %86, i64 %85
  %88 = bitcast %class.KV.9* %87 to i8*
  %89 = load i64, i64* %15, align 8
  %90 = add i64 %89, 1
  %91 = load %class.KV.9*, %class.KV.9** %10, align 8
  %92 = getelementptr inbounds %class.KV.9, %class.KV.9* %91, i64 %90
  %93 = bitcast %class.KV.9* %92 to i8*
  %94 = load i64, i64* %13, align 8
  %95 = sub i64 %94, 1
  %96 = load i64, i64* %15, align 8
  %97 = sub i64 %95, %96
  %98 = mul i64 %97, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %88, i8* %93, i64 %98, i32 8, i1 false)
  %99 = load i64, i64* %11, align 8
  %100 = load i64, i64* %12, align 8
  %101 = shl i64 1, %100
  %102 = xor i64 -1, %101
  %103 = and i64 %99, %102
  %104 = shl i64 %103, 1
  %105 = or i64 %104, 1
  store i64 %105, i64* %17, align 8
  %106 = load i64, i64* %17, align 8
  %107 = load %class.KV.9*, %class.KV.9** %16, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %106, %class.KV.9* %107)
  br label %136

; <label>:108                                     ; preds = %61
  %109 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %109)
  br label %136

; <label>:110                                     ; preds = %44
  %111 = load i64, i64* %15, align 8
  %112 = load %class.KV.9*, %class.KV.9** %10, align 8
  %113 = getelementptr inbounds %class.KV.9, %class.KV.9* %112, i64 %111
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 6
  %116 = load %class.key*, %class.key** %8, align 8
  %117 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3key5valueLj10EE12remove_innerERKS2_yPKS0_Py(%class.KV.9* sret %18, %class.KV.9* dereferenceable(16) %113, i64 %115, %class.key* %116, i64* %117)
  %118 = load i64, i64* %15, align 8
  %119 = load %class.KV.9*, %class.KV.9** %10, align 8
  %120 = getelementptr inbounds %class.KV.9, %class.KV.9* %119, i64 %118
  %121 = call zeroext i1 @_ZNK2KVI3key5valueLj10EEeqERKS2_(%class.KV.9* %18, %class.KV.9* dereferenceable(16) %120)
  br i1 %121, label %122, label %124

; <label>:122                                     ; preds = %110
  %123 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %123)
  br label %136

; <label>:124                                     ; preds = %110
  %125 = load %class.KV.9*, %class.KV.9** %10, align 8
  %126 = load i64, i64* %13, align 8
  %127 = load i64, i64* %15, align 8
  %128 = call %class.KV.9* @_ZN2KVI3key5valueLj10EE11update_nodeEPKS2_mmRS3_(%class.KV.9* %125, i64 %126, i64 %127, %class.KV.9* dereferenceable(16) %18)
  store %class.KV.9* %128, %class.KV.9** %19, align 8
  %129 = load %class.KV.8*, %class.KV.8** %6, align 8
  %130 = getelementptr inbounds %class.KV.8, %class.KV.8* %129, i32 0, i32 0
  %131 = bitcast %"union.KV<key, value, 9>::Key"* %130 to i64*
  %132 = load i64, i64* %131, align 8
  %133 = load %class.KV.9*, %class.KV.9** %19, align 8
  call void @_ZN2KVI3key5valueLj9EEC1EyPKS_IS0_S1_Lj10EE(%class.KV.8* %0, i64 %132, %class.KV.9* %133)
  br label %136

; <label>:134                                     ; preds = %5
  %135 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3key5valueLj9EEC1ERKS2_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %135)
  br label %136

; <label>:136                                     ; preds = %134, %124, %122, %108, %70
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj9EEeqERKS2_(%class.KV.8*, %class.KV.8* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.8*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %class.KV.8*, %class.KV.8** %3, align 8
  %6 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 9>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.8*, %class.KV.8** %4, align 8
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 9>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 9>::Val"* %15 to %class.KV.9**
  %17 = load %class.KV.9*, %class.KV.9** %16, align 8
  %18 = load %class.KV.8*, %class.KV.8** %4, align 8
  %19 = getelementptr inbounds %class.KV.8, %class.KV.8* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 9>::Val"* %19 to %class.KV.9**
  %21 = load %class.KV.9*, %class.KV.9** %20, align 8
  %22 = icmp eq %class.KV.9* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj10EE12remove_innerERKS2_yPKS0_Py(%class.KV.9* noalias sret, %class.KV.9* dereferenceable(16), i64, %class.key*, i64*) #0 align 2 {
  %6 = alloca %class.KV.9*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.LL*, align 8
  store %class.KV.9* %1, %class.KV.9** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %11 = load %class.KV.9*, %class.KV.9** %6, align 8
  %12 = getelementptr inbounds %class.KV.9, %class.KV.9* %11, i32 0, i32 1
  %13 = bitcast %"union.KV<key, value, 10>::Val"* %12 to %class.LL**
  %14 = load %class.LL*, %class.LL** %13, align 8
  %15 = icmp ne %class.LL* %14, null
  br i1 %15, label %16, label %34

; <label>:16                                      ; preds = %5
  %17 = load %class.KV.9*, %class.KV.9** %6, align 8
  %18 = getelementptr inbounds %class.KV.9, %class.KV.9* %17, i32 0, i32 1
  %19 = bitcast %"union.KV<key, value, 10>::Val"* %18 to %class.LL**
  %20 = load %class.LL*, %class.LL** %19, align 8
  %21 = load %class.key*, %class.key** %8, align 8
  %22 = load i64*, i64** %9, align 8
  %23 = call %class.LL* @_ZNK2LLI3key5valueE6removeEPKS0_Py(%class.LL* %20, %class.key* %21, i64* %22)
  store %class.LL* %23, %class.LL** %10, align 8
  %24 = load %class.LL*, %class.LL** %10, align 8
  %25 = load %class.KV.9*, %class.KV.9** %6, align 8
  %26 = getelementptr inbounds %class.KV.9, %class.KV.9* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, value, 10>::Val"* %26 to %class.LL**
  %28 = load %class.LL*, %class.LL** %27, align 8
  %29 = icmp eq %class.LL* %24, %28
  br i1 %29, label %30, label %32

; <label>:30                                      ; preds = %16
  %31 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3key5valueLj10EEC1ERKS2_(%class.KV.9* %0, %class.KV.9* dereferenceable(16) %31)
  br label %36

; <label>:32                                      ; preds = %16
  %33 = load %class.LL*, %class.LL** %10, align 8
  call void @_ZN2KVI3key5valueLj10EEC1EyPK2LLIS0_S1_E(%class.KV.9* %0, i64 1, %class.LL* %33)
  br label %36

; <label>:34                                      ; preds = %5
  %35 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3key5valueLj10EEC1ERKS2_(%class.KV.9* %0, %class.KV.9* dereferenceable(16) %35)
  br label %36

; <label>:36                                      ; preds = %34, %32, %30
  ret void
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3key5valueLj10EEeqERKS2_(%class.KV.9*, %class.KV.9* dereferenceable(16)) #5 align 2 {
  %3 = alloca %class.KV.9*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %class.KV.9*, %class.KV.9** %3, align 8
  %6 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, value, 10>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.9*, %class.KV.9** %4, align 8
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, value, 10>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14                                      ; preds = %2
  %15 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, value, 10>::Val"* %15 to %class.value**
  %17 = load %class.value*, %class.value** %16, align 8
  %18 = load %class.KV.9*, %class.KV.9** %4, align 8
  %19 = getelementptr inbounds %class.KV.9, %class.KV.9* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, value, 10>::Val"* %19 to %class.value**
  %21 = load %class.value*, %class.value** %20, align 8
  %22 = icmp eq %class.value* %17, %21
  br label %23

; <label>:23                                      ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: ssp uwtable
define linkonce_odr %class.LL* @_ZNK2LLI3key5valueE6removeEPKS0_Py(%class.LL*, %class.key*, i64*) #0 align 2 {
  %4 = alloca %class.LL*, align 8
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca i64*, align 8
  %8 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %5, align 8
  store %class.key* %1, %class.key** %6, align 8
  store i64* %2, i64** %7, align 8
  %9 = load %class.LL*, %class.LL** %5, align 8
  %10 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %11 = load %class.key*, %class.key** %10, align 8
  %12 = load %class.key*, %class.key** %6, align 8
  %13 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %11, %class.key* dereferenceable(8) %12)
  br i1 %13, label %14, label %20

; <label>:14                                      ; preds = %3
  %15 = load i64*, i64** %7, align 8
  %16 = load i64, i64* %15, align 8
  %17 = add i64 %16, -1
  store i64 %17, i64* %15, align 8
  %18 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %19 = load %class.LL*, %class.LL** %18, align 8
  store %class.LL* %19, %class.LL** %4, align 8
  br label %46

; <label>:20                                      ; preds = %3
  %21 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %22 = load %class.LL*, %class.LL** %21, align 8
  %23 = icmp ne %class.LL* %22, null
  br i1 %23, label %24, label %45

; <label>:24                                      ; preds = %20
  %25 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %26 = load %class.LL*, %class.LL** %25, align 8
  %27 = load %class.key*, %class.key** %6, align 8
  %28 = load i64*, i64** %7, align 8
  %29 = call %class.LL* @_ZNK2LLI3key5valueE6removeEPKS0_Py(%class.LL* %26, %class.key* %27, i64* %28)
  store %class.LL* %29, %class.LL** %8, align 8
  %30 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %31 = load %class.LL*, %class.LL** %30, align 8
  %32 = load %class.LL*, %class.LL** %8, align 8
  %33 = icmp eq %class.LL* %31, %32
  br i1 %33, label %34, label %35

; <label>:34                                      ; preds = %24
  store %class.LL* %9, %class.LL** %4, align 8
  br label %46

; <label>:35                                      ; preds = %24
  %36 = call i8* @malloc(i64 24)
  %37 = bitcast i8* %36 to %class.LL*
  %38 = bitcast %class.LL* %37 to i8*
  %39 = bitcast i8* %38 to %class.LL*
  %40 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %41 = load %class.key*, %class.key** %40, align 8
  %42 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 1
  %43 = load %class.value*, %class.value** %42, align 8
  %44 = load %class.LL*, %class.LL** %8, align 8
  call void @_ZN2LLI3key5valueEC1EPKS0_PKS1_PKS2_(%class.LL* %39, %class.key* %41, %class.value* %43, %class.LL* %44)
  store %class.LL* %39, %class.LL** %4, align 8
  br label %46

; <label>:45                                      ; preds = %20
  store %class.LL* %9, %class.LL** %4, align 8
  br label %46

; <label>:46                                      ; preds = %45, %35, %34, %14
  %47 = load %class.LL*, %class.LL** %4, align 8
  ret %class.LL* %47
}

; Function Attrs: nounwind ssp uwtable
define linkonce_odr void @_ZN2KVI3key5valueLj0EEC2ERKS2_(%class.KV*, %class.KV* dereferenceable(16)) unnamed_addr #5 align 2 {
  %3 = alloca %class.KV*, align 8
  %4 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %3, align 8
  store %class.KV* %1, %class.KV** %4, align 8
  %5 = load %class.KV*, %class.KV** %3, align 8
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 0
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, value, 0>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, value, 0>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 1
  %12 = load %class.KV*, %class.KV** %4, align 8
  %13 = getelementptr inbounds %class.KV, %class.KV* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, value, 0>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, value, 0>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

attributes #0 = { ssp uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { nobuiltin "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind ssp uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="core2" "target-features"="+cx16,+fxsr,+mmx,+sse,+sse2,+sse3,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { argmemonly nounwind }
attributes #8 = { builtin }
attributes #9 = { noreturn }
attributes #10 = { builtin nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"PIC Level", i32 2}
!1 = !{!"Apple LLVM version 8.0.0 (clang-800.0.42.1)"}
