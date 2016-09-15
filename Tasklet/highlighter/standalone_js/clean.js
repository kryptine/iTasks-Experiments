function __StdList_flatten$eval(a0) {
    return __StdList_flatten(Sapl.feval(a0));
};

function __StdList_flatten(___x_0) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var __h_1_0_1 = ys[2],
                __t_1_1_1 = ys[3];
            return __StdList_$2B$2B(Sapl.feval(__h_1_0_1), [__StdList_flatten$eval, [__t_1_1_1]]);
        case 1:
            return ___predefined__Nil();
    }
};

function __EdTab_getAnnotations(__pic_0) {
    var ys = Sapl.feval(__pic_0);
    var __pencolor_1_0_1 = ys[2],
        __len_1_1_1 = ys[3],
        __out_1_2_1 = ys[4];
    return ___predefined__Tuple2([__StdList_reverse$eval, [__out_1_2_1]], __pic_0);
};

function __Highlighter_genAnnotations(___x_0) {
    var ys = Sapl.feval(___x_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2],
                __ls_1_1_1 = ys[3];
            return ___predefined__Cons([___predefined_tupsels2v0, [
                [__EdTab_getAnnotations, [
                    [__EdTab_tabDrawStringC$eval, [__l_1_0_1, __EdTab_DefaultSyntaxColours, __EdTab_newPicture]]
                ]]
            ]], [__Highlighter_genAnnotations, [__ls_1_1_1]]);
        case 1:
            return ___predefined__Nil();
    }
};

function __syncol__f55_55_def0(___x_0, __state_1, __state$60_2) {
    if ((!__syncol_get_typedecl_7(__state_1))) {
        return __syncol_get_typedecl_7(__state$60_2);
    } else {
        return false;
    }
};

function __syncol__f55_55(___x_0, __state_1, __state$60_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return true;
    } else {
        return __syncol__f55_55_def0(___x_0, __state_1, __state$60_2);
    }
};

function __syncol_backpatch_20_select0(__info_3_0, __state$60_2_0, __line_2_1, ___x_1_0, __lines_1_1, __state_0, __res_1, __acc_2, ___x_3) {
    if ((!__syncol_get_typedecl_7(__state_0))) {
        return __syncol_get_typedecl_7(__state$60_2_0);
    } else {
        return false;
    }
};

function __syncol_accum(__acc_0, __info_1) {
    return __StdList_$2B$2B(Sapl.feval(__acc_0), ___predefined__Cons(__info_1, ___predefined__Nil()));
};

function __syncol_backpatch_20_select1(__info_3_0, __state$60_2_0, __line_2_1, ___x_1_0, __lines_1_1, __state_0, __res_1, __acc_2, ___x_3) {
    if (__syncol_backpatch_20_select0(__info_3_0, __state$60_2_0, __line_2_1, ___x_1_0, __lines_1_1, __state_0, __res_1, __acc_2, ___x_3)) {
        return __syncol_backpatch_20(__state$60_2_0, [__syncol_patch, [__res_1, __acc_2]], ___predefined__Cons(___predefined__Tuple2(__info_3_0, __line_2_1), ___predefined__Nil()), __lines_1_1);
    } else {
        return __syncol_backpatch_20(__state$60_2_0, __res_1, [__syncol_accum, [__acc_2, ___predefined__Tuple2(__info_3_0, __line_2_1)]], __lines_1_1);
    }
};

function __syncol_backpatch_20_select2(__info_3_0, __state$60_2_0, __line_2_1, ___x_1_0, __lines_1_1, __state_0, __res_1, __acc_2, ___x_3) {
    if (__syncol_get_has_content_10(__state$60_2_0)) {
        return __syncol_backpatch_20(__state$60_2_0, [__syncol_copy, [__res_1, __acc_2]], ___predefined__Cons(___predefined__Tuple2(__info_3_0, __line_2_1), ___predefined__Nil()), __lines_1_1);
    } else {
        return __syncol_backpatch_20_select1(__info_3_0, __state$60_2_0, __line_2_1, ___x_1_0, __lines_1_1, __state_0, __res_1, __acc_2, ___x_3);
    }
};

function __syncol_backpatch_20(__state_0, __res_1, __acc_2, ___x_3) {
    var ys = Sapl.feval(___x_3);
    switch (ys[0]) {
        case 0:
            var ___x_1_0_1 = ys[2],
                __lines_1_1_1 = ys[3];
            var ys = Sapl.feval(___x_1_0_1);
            var __state$60_2_0_2 = ys[2],
                __line_2_1_2 = ys[3];
            var __info_3_0_3 = __syncol__Info([__syncol_get_level_5, [__state_0]], [__syncol_get_typedef_6, [__state$60_2_0_2]], [__syncol_get_typedecl_7, [__state$60_2_0_2]], [__syncol_get_offside_8, [__state$60_2_0_2]], [__syncol__f55_55, [
                [__syncol_get_has_content_10, [__state$60_2_0_2]], __state_0, __state$60_2_0_2]]);
            return __syncol_backpatch_20_select2(__info_3_0_3, __state$60_2_0_2, __line_2_1_2, ___x_1_0_1, __lines_1_1_1, __state_0, __res_1, __acc_2, ___x_3);;
        case 1:
            return Sapl.feval([__res_1, [__acc_2]]);
    }
};

function __StdFunc_id$eval(a0) {
    return __StdFunc_id(Sapl.feval(a0));
};

function __StdFunc_id(__x_0) {
    return __x_0;
};

function __syncol_parse_19(__state_0, ___x_1) {
    var ys = Sapl.feval(___x_1);
    switch (ys[0]) {
        case 0:
            var __line_1_0_1 = ys[2],
                __lines_1_1_1 = ys[3];
            var __state_2_0_2 = [__syncol_parseLine, [__state_0, __line_1_0_1]];
            return ___predefined__Cons(___predefined__Tuple2(__state_2_0_2, __line_1_0_1), [__syncol_parse_19, [__state_2_0_2, __lines_1_1_1]]);;
        case 1:
            return ___predefined__Nil();
    }
};

function __syncol_firstParse$eval(a0) {
    return __syncol_firstParse(Sapl.feval(a0));
};

function __syncol_firstParse(__lines_0) {
    return __StrictList_slFromList(__syncol_backpatch_20(__syncol_iniState, __StdFunc_id$eval, ___predefined__Nil(), [__syncol_parse_19, [__syncol_iniState, __lines_0]]));
};

function __Highlighter_toStrictList(___x_0) {
    var ys = Sapl.feval(___x_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2],
                __ls_1_1_1 = ys[3];
            return __StrictList_SCons(__l_1_0_1, [__Highlighter_toStrictList, [__ls_1_1_1]]);
        case 1:
            return __StrictList_SNil();
    }
};

function __Text_equalStringOrIndexOfNext_29$eval(a0, a1, a2, a3, a4) {
    return __Text_equalStringOrIndexOfNext_29(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4));
};

function __Text_equalStringOrIndexOfNext_29(__i_0, __offs_1, __max_offs_2, __needle_3, __haystack_4) {
    while (1) {
        if (__StdInt_$3C_17(__i_0, ___SystemArray_size_20(__needle_3))) {
            if ((___SystemArray_select_19(__needle_3, __i_0) == ___SystemArray_select_19(__haystack_4, [__add, [__i_0, __offs_1]]))) {
                var t0 = (__i_0 + 1),
                    t1 = __offs_1,
                    t2 = __max_offs_2,
                    t3 = __needle_3,
                    t4 = __haystack_4;
                __i_0 = t0;
                __offs_1 = t1;
                __max_offs_2 = t2;
                __needle_3 = t3;
                __haystack_4 = t4;
                continue;
            } else {
                return __Text_indexOf$60_28((__offs_1 + 1), ___SystemArray_select_19(__needle_3, 0), __max_offs_2, __needle_3, __haystack_4);
            }
        } else {
            return __offs_1;
        }
    }
};

function __Text_indexOf$60_28$eval(a0, a1, a2, a3, a4) {
    return __Text_indexOf$60_28(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4));
};

function __Text_indexOf$60_28(__offs_0, __needleChar0_1, __max_offs_2, __needle_3, __haystack_4) {
    while (1) {
        if ((!__StdInt_$3C_17(__max_offs_2, __offs_0))) {
            if ((!(___SystemArray_select_19(__haystack_4, __offs_0) == __needleChar0_1))) {
                var t0 = (__offs_0 + 1),
                    t1 = __needleChar0_1,
                    t2 = __max_offs_2,
                    t3 = __needle_3,
                    t4 = __haystack_4;
                __offs_0 = t0;
                __needleChar0_1 = t1;
                __max_offs_2 = t2;
                __needle_3 = t3;
                __haystack_4 = t4;
                continue;
            } else {
                return __Text_equalStringOrIndexOfNext_29(1, __offs_0, __max_offs_2, __needle_3, __haystack_4);
            }
        } else {
            return -1;
        }
    }
};

function __Text_indexOfAfter_3$eval(a0, a1, a2) {
    return __Text_indexOfAfter_3(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __Text_indexOfAfter_3(__offs_0, __needle_1, __haystack_2) {
    if ((___SystemArray_size_20(__needle_1) == 0)) {
        return -1;
    } else {
        return __Text_indexOf$60_28(__offs_0, ___SystemArray_select_19(__needle_1, 0), (___SystemArray_size_20(__haystack_2) - ___SystemArray_size_20(__needle_1)), __needle_1, __haystack_2);
    }
};

function __Text_splitAfter_24_select0$eval(a0, a1, a2, a3, a4) {
    return __Text_splitAfter_24_select0(a0, Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4));
};

function __Text_splitAfter_24_select0(__index_1_0, __offs_0, __end_1, __sep_2, __s_3) {
    if ((Sapl.feval(__index_1_0) == -1)) {
        return ___predefined__Cons([__StdString_$7C_10, [__s_3, ___predefined__Tuple2(__offs_0, __end_1)]], ___predefined__Nil());
    } else {
        return ___predefined__Cons([__StdString_$7C_10, [__s_3, ___predefined__Tuple2(__offs_0, [__sub, [__index_1_0, 1]])]], [__Text_splitAfter_24$eval, [
            [__add, [__index_1_0, [___SystemArray_size_20, [__sep_2]]]], __end_1, __sep_2, __s_3]]);
    }
};

function __Text_splitAfter_24$eval(a0, a1, a2, a3) {
    return __Text_splitAfter_24(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3));
};

function __Text_splitAfter_24(__offs_0, __end_1, __sep_2, __s_3) {
    var __index_1_0_1 = [__Text_indexOfAfter_3$eval, [__offs_0, __sep_2, __s_3]];
    return __Text_splitAfter_24_select0(__index_1_0_1, __offs_0, __end_1, __sep_2, __s_3);;
};

function __Text_split_11$eval(a0, a1) {
    return __Text_split_11(Sapl.feval(a0), Sapl.feval(a1));
};

function __Text_split_11(__sep_0, __s_1) {
    return __Text_splitAfter_24(0, (___SystemArray_size_20(__s_1) - 1), __sep_0, __s_1);
};

function __StdList_acclen_111(__n_0, ___x_1) {
    while (1) {
        var ys = Sapl.feval(___x_1);
        switch (ys[0]) {
            case 0:
                var __x_1_0_1 = ys[2],
                    __xs_1_1_1 = ys[3];
                var t0 = [__add, [__n_0, 1]],
                    t1 = __xs_1_1_1;
                __n_0 = t0;
                ___x_1 = t1;
                continue;
            case 1:
                return Sapl.feval(__n_0);
        }
    }
};

function __StdList_length_47$eval(a0) {
    return __StdList_length_47(Sapl.feval(a0));
};

function __StdList_length_47(__xs_0) {
    return __StdList_acclen_111(0, __xs_0);
};

function __syncol__f62_62_def0(___x_0, __state_1, __state$60_2) {
    if ((!__syncol_get_typedecl_7(__state_1))) {
        return __syncol_get_typedecl_7(__state$60_2);
    } else {
        return false;
    }
};

function __syncol__f62_62(___x_0, __state_1, __state$60_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return true;
    } else {
        return __syncol__f62_62_def0(___x_0, __state_1, __state$60_2);
    }
};

function __syncol_eqInfo$eval(a0, a1) {
    return __syncol_eqInfo(Sapl.feval(a0), Sapl.feval(a1));
};

function __syncol_eqInfo(___x_0, ___x_1) {
    var ys = ___x_0;
    var __a_1_0_1 = ys[2],
        __b_1_1_1 = ys[3],
        __c_1_2_1 = ys[4],
        __d_1_3_1 = ys[5],
        __e_1_4_1 = ys[6];
    var ys = ___x_1;
    var __a$60_2_0_2 = ys[2],
        __b$60_2_1_2 = ys[3],
        __c$60_2_2_2 = ys[4],
        __d$60_2_3_2 = ys[5],
        __e$60_2_4_2 = ys[6];
    if ((Sapl.feval(__a_1_0_1) == Sapl.feval(__a$60_2_0_2))) {
        if ((Sapl.feval(__b_1_1_1) == Sapl.feval(__b$60_2_1_2))) {
            if ((Sapl.feval(__c_1_2_1) == Sapl.feval(__c$60_2_2_2))) {
                if ((Sapl.feval(__d_1_3_1) == Sapl.feval(__d$60_2_3_2))) {
                    return (Sapl.feval(__e_1_4_1) == Sapl.feval(__e$60_2_4_2));
                } else {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }
};

function __StrictList_slToList$eval(a0) {
    return __StrictList_slToList(Sapl.feval(a0));
};

function __StrictList_slToList(___x_0) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var __x_1_0_1 = ys[2],
                __xs_1_1_1 = ys[3];
            return ___predefined__Cons(__x_1_0_1, [__StrictList_slToList$eval, [__xs_1_1_1]]);
        case 1:
            return ___predefined__Nil();
    }
};

function __syncol_after_41_select0(__flush$60_4_0, __info$60_4_1, __state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5) {
    if (__syncol_eqInfo(Sapl.feval(__info_2_0), Sapl.feval(__info$60_4_1))) {
        return ___predefined__Tuple3(__beg_0, __idx_1, [__res_3, [
            [__StdList_$2B$2B$eval, [__acc_4, [__StrictList_slToList$eval, [___x_5]]]]
        ]]);
    } else {
        return __syncol_after_41(__beg_0, [__add, [__idx_1, 1]], __state$60_3_0, [__syncol_copy, [__res_3, __acc_4]], ___predefined__Cons(___predefined__Tuple2(__info$60_4_1, __line_2_1), ___predefined__Nil()), __lines_1_1);
    }
};

function __syncol_after_41_select1(__state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5) {
    if ((!__syncol_get_typedecl_7(__state_2))) {
        return __syncol_get_typedecl_7(__state$60_3_0);
    } else {
        return false;
    }
};

function __syncol__f63_63_def0(___x_0, __state_1, __state$60_2) {
    if ((!__syncol_get_typedecl_7(__state_1))) {
        return __syncol_get_typedecl_7(__state$60_2);
    } else {
        return false;
    }
};

function __syncol__f63_63(___x_0, __state_1, __state$60_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return true;
    } else {
        return __syncol__f63_63_def0(___x_0, __state_1, __state$60_2);
    }
};

function __syncol_after_41_select2(__state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5) {
    if (__syncol_after_41_select1(__state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5)) {
        return __syncol_after_41(__beg_0, __idx_1, [__syncol_set_typedecl_7, [__state_2, true]], [__syncol_patch, [__res_3, __acc_4]], ___predefined__Nil(), ___x_5);
    } else {
        return __syncol_after_41(__beg_0, [__add, [__idx_1, 1]], __state$60_3_0, __res_3, [__StdList_$2B$2B$eval, [__acc_4, ___predefined__Cons(___predefined__Tuple2(__syncol__Info([__syncol_get_level_5, [__state_2]], [__syncol_get_typedef_6, [__state$60_3_0]], [__syncol_get_typedecl_7, [__state$60_3_0]], [__syncol_get_offside_8, [__state$60_3_0]], [__syncol__f63_63, [
            [__syncol_get_has_content_10, [__state$60_3_0]], __state_2, __state$60_3_0]]), __line_2_1), ___predefined__Nil())]], __lines_1_1);
    }
};

function __syncol_after_41_select3(__state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5) {
    if (__syncol_get_has_content_10(__state$60_3_0)) {
        var __flush$60_4_0_1 = [__syncol__f62_62, [
            [__syncol_get_has_content_10, [__state$60_3_0]], __state_2, __state$60_3_0]],
            __info$60_4_1_1 = __syncol__Info([__syncol_get_level_5, [__state_2]], [__syncol_get_typedef_6, [__state$60_3_0]], [__syncol_get_typedecl_7, [__state$60_3_0]], [__syncol_get_offside_8, [__state$60_3_0]], __flush$60_4_0_1);
        return __syncol_after_41_select0(__flush$60_4_0_1, __info$60_4_1_1, __state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5);;
    } else {
        return __syncol_after_41_select2(__state$60_3_0, __info_2_0, __line_2_1, ___x_1_0, __lines_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5);
    }
};

function __syncol_after_41(__beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5) {
    var ys = Sapl.feval(___x_5);
    switch (ys[0]) {
        case 0:
            var ___x_1_0_1 = ys[2],
                __lines_1_1_1 = ys[3];
            var ys = Sapl.feval(___x_1_0_1);
            var __info_2_0_2 = ys[2],
                __line_2_1_2 = ys[3];
            var __state$60_3_0_3 = [__syncol_parseLine, [__state_2, __line_2_1_2]];
            return __syncol_after_41_select3(__state$60_3_0_3, __info_2_0_2, __line_2_1_2, ___x_1_0_1, __lines_1_1_1, __beg_0, __idx_1, __state_2, __res_3, __acc_4, ___x_5);;
        case 1:
            return ___predefined__Tuple3(__beg_0, [__sub, [__idx_1, 1]], [__res_3, [__acc_4]]);
    }
};

function __StdInt_$3C$3C(__a_0, __b_1) {
    return (Sapl.feval(__a_0) << Sapl.feval(__b_1));
};

function __StdInt_$3E$3E(__a_0, __b_1) {
    return (Sapl.feval(__a_0) >>> Sapl.feval(__b_1));
};

function __syncol_scanFirst_27_select0$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_scanFirst_27_select0(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_scanFirst_27_select0(__index$60$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if (__syncol_funnyChar([___SystemArray_select_19, [__line_4, __index$60$60_3_0]])) {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    } else {
        return __syncol_scanFirst_27(__line_size_0, (__level_1 - 1), Sapl.feval(__index$60$60_3_0), ((__indent_3 + 1) + 1), __line_4);
    }
};

function __syncol_scanFirst_27_select1$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_scanFirst_27_select1(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_scanFirst_27_select1(__index$60$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!__StdInt_$3C_17(Sapl.feval(__index$60$60_3_0), Sapl.feval(__line_size_0)))) {
        return ___predefined__Tuple3(__index$60$60_3_0, -1, [__sub, [__level_1, 1]]);
    } else {
        return __syncol_scanFirst_27_select0(__index$60$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select2$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_scanFirst_27_select2(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_scanFirst_27_select2(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!(__level_1 == 0))) {
        return __syncol_scanFirst_27(__line_size_0, (__level_1 - 1), (Sapl.feval(__index$60_2_0) + 1), ((__indent_3 + 1) + 1), __line_4);
    } else {
        var __index$60$60_3_0_1 = [__add, [__index$60_2_0, 1]];
        return __syncol_scanFirst_27_select1(__index$60$60_3_0_1, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);;
    }
};

function __syncol_scanFirst_27_select3$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_scanFirst_27_select3(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_scanFirst_27_select3(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((__level_1 == 0)) {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    } else {
        return __syncol_scanFirst_27(__line_size_0, __level_1, Sapl.feval(__index$60_2_0), (__indent_3 + 1), __line_4);
    }
};

function __syncol_scanFirst_27_select4$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_scanFirst_27_select4(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_scanFirst_27_select4(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((___SystemArray_select_19(__line_4, __index$60_2_0) == '/')) {
        return __syncol_scanFirst_27_select2(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    } else {
        return __syncol_scanFirst_27_select3(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select5$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_scanFirst_27_select5(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_scanFirst_27_select5(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!__StdInt_$3C_17(Sapl.feval(__index$60_2_0), Sapl.feval(__line_size_0)))) {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    } else {
        return __syncol_scanFirst_27_select4(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select6$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_scanFirst_27_select6(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_scanFirst_27_select6(__char$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((__level_1 == 0)) {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    } else {
        return __syncol_scanFirst_27(__line_size_0, __level_1, Sapl.feval(__index$60_2_0), (__indent_3 + 1), __line_4);
    }
};

function __syncol_scanFirst_27_select7$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_scanFirst_27_select7(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_scanFirst_27_select7(__char$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char$60_3_0) == '*')) {
        return __syncol_scanFirst_27(__line_size_0, (__level_1 + 1), (Sapl.feval(__index$60_2_0) + 1), ((__indent_3 + 1) + 1), __line_4);
    } else {
        return __syncol_scanFirst_27_select6(__char$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select8$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_scanFirst_27_select8(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_scanFirst_27_select8(__char$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char$60_3_0) == '/')) {
        return ___predefined__Tuple3(__index_2, -1, __level_1);
    } else {
        return __syncol_scanFirst_27_select7(__char$60_3_0, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select9$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_scanFirst_27_select9(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_scanFirst_27_select9(__index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!__StdInt_$3C_17(Sapl.feval(__index$60_2_0), Sapl.feval(__line_size_0)))) {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    } else {
        var __char$60_3_0_1 = [___SystemArray_select_19, [__line_4, __index$60_2_0]];
        return __syncol_scanFirst_27_select8(__char$60_3_0_1, __index$60_2_0, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);;
    }
};

function __syncol_scanFirst_27_select10$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select10(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select10(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!(__level_1 == 0))) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), (__indent_3 + 1), __line_4);
    } else {
        return ___predefined__Tuple3(__index_2, __indent_3, __level_1);
    }
};

function __syncol_scanFirst_27_select11$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select11(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select11(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '/')) {
        var __index$60_2_0_1 = [__add, [__index_2, 1]];
        return __syncol_scanFirst_27_select9(__index$60_2_0_1, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);;
    } else {
        return __syncol_scanFirst_27_select10(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select12$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select12(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select12(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '*')) {
        var __index$60_2_0_1 = [__add, [__index_2, 1]];
        return __syncol_scanFirst_27_select5(__index$60_2_0_1, __char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);;
    } else {
        return __syncol_scanFirst_27_select11(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select13$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select13(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select13(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '\f')) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), __indent_3, __line_4);
    } else {
        return __syncol_scanFirst_27_select12(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select14$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select14(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select14(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '\r')) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), __indent_3, __line_4);
    } else {
        return __syncol_scanFirst_27_select13(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select15$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select15(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select15(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '\n')) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), __indent_3, __line_4);
    } else {
        return __syncol_scanFirst_27_select14(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select16$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select16(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select16(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == '\t')) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), __StdInt_$3C$3C([__add, [
            [__StdInt_$3E$3E, [__indent_3, 2]], 1]], 2), __line_4);
    } else {
        return __syncol_scanFirst_27_select15(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27_select17$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_scanFirst_27_select17(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_scanFirst_27_select17(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((Sapl.feval(__char_1_0) == ' ')) {
        return __syncol_scanFirst_27(__line_size_0, __level_1, (__index_2 + 1), (__indent_3 + 1), __line_4);
    } else {
        return __syncol_scanFirst_27_select16(__char_1_0, __line_size_0, __level_1, __index_2, __indent_3, __line_4);
    }
};

function __syncol_scanFirst_27$eval(a0, a1, a2, a3, a4) {
    return __syncol_scanFirst_27(a0, Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4));
};

function __syncol_scanFirst_27(__line_size_0, __level_1, __index_2, __indent_3, __line_4) {
    if ((!__StdInt_$3C_17(__index_2, Sapl.feval(__line_size_0)))) {
        return ___predefined__Tuple3(__index_2, -1, __level_1);
    } else {
        var __char_1_0_1 = [___SystemArray_select_19, [__line_4, __index_2]];
        return __syncol_scanFirst_27_select17(__char_1_0_1, __line_size_0, __level_1, __index_2, __indent_3, __line_4);;
    }
};

function __syncol_scanFirst$eval(a0, a1) {
    return __syncol_scanFirst(Sapl.feval(a0), Sapl.feval(a1));
};

function __syncol_scanFirst(__level_0, __line_1) {
    return __syncol_scanFirst_27([___SystemArray_size_20, [__line_1]], __level_0, 0, 0, __line_1);
};

function __syncol__if$3B165$3B31_48_select0(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5) {
    if ((!__StdInt_$3C_17(Sapl.feval(__indent_3), 0))) {
        return (!__StdInt_$3C_17(Sapl.feval(__offside_4), Sapl.feval(__indent_3)));
    } else {
        return false;
    }
};

function __syncol__if$3B165$3B31_48_select1(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5) {
    if (__StdInt_$3C_17(Sapl.feval(__index_1), Sapl.feval(__line_size_2))) {
        return __syncol__if$3B165$3B31_48_select0(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5);
    } else {
        return false;
    }
};

function __syncol__c$3B170$3B58_46_def0(__parse_state_0, __offside_1, __indent_2) {
    return Sapl.feval(__indent_2);
};

function __syncol__c$3B170$3B58_46(__parse_state_0, __offside_1, __indent_2) {
    var ys = Sapl.feval(__parse_state_0);
    switch (ys[0]) {
        case 0:
            return __syncol__c$3B170$3B58_46_def0(__parse_state_0, __offside_1, __indent_2);
        case 1:
            return Sapl.feval(__offside_1);
        case 2:
            return Sapl.feval(__offside_1);
        case 3:
            return Sapl.feval(__offside_1);
        case 4:
            return Sapl.feval(__offside_1);
        case 5:
            return Sapl.feval(__offside_1);
        case 6:
            return __syncol__c$3B170$3B58_46_def0(__parse_state_0, __offside_1, __indent_2);
    }
};

function __syncol__if$3B170$3B40_47_def0(___x_0, __parse_state_1, __offside_2, __indent_3) {
    return Sapl.feval(__offside_2);
};

function __syncol__if$3B170$3B40_47(___x_0, __parse_state_1, __offside_2, __indent_3) {
    if ((Sapl.feval(___x_0) == true)) {
        return __syncol__c$3B170$3B58_46(__parse_state_1, __offside_2, __indent_3);
    } else {
        return __syncol__if$3B170$3B40_47_def0(___x_0, __parse_state_1, __offside_2, __indent_3);
    }
};

function __syncol__if$3B165$3B31_48_def0(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5) {
    return ___predefined__Tuple2(false, [__syncol__if$3B170$3B40_47, [
        [__not, [
            [__StdInt_$3C_17$eval, [__indent_3, 0]]
        ]], __parse_state_5, __offside_4, __indent_3]]);
};

function __syncol__if$3B165$3B31_48(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5) {
    if ((Sapl.feval(__typedecl_0) == true)) {
        if (__syncol__if$3B165$3B31_48_select1(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5)) {
            return ___predefined__Tuple2(false, __indent_3);
        } else {
            return ___predefined__Tuple2(true, __offside_4);
        }
    } else {
        return __syncol__if$3B165$3B31_48_def0(__typedecl_0, __index_1, __line_size_2, __indent_3, __offside_4, __parse_state_5);
    }
};

function __syncol__c$3B180$3B34_49_def0(__parse_state_0) {
    return __syncol_StartOfBlock();
};

function __syncol__c$3B180$3B34_49(__parse_state_0) {
    var ys = Sapl.feval(__parse_state_0);
    switch (ys[0]) {
        case 0:
            return __syncol__c$3B180$3B34_49_def0(__parse_state_0);
        case 1:
            return __syncol_CleanId();
        case 2:
            return __syncol_OpenPar();
        case 3:
            return __syncol_InfixId();
        case 4:
            return __syncol_Precedence();
        case 5:
            return __syncol_Fixity();
        case 6:
            return __syncol__c$3B180$3B34_49_def0(__parse_state_0);
    }
};

function __syncol__if$3B179$3B31_50_def0(___x_0, __parse_state_1) {
    return Sapl.feval(__parse_state_1);
};

function __syncol__if$3B179$3B31_50(___x_0, __parse_state_1) {
    if ((Sapl.feval(___x_0) == true)) {
        return __syncol__c$3B180$3B34_49(__parse_state_1);
    } else {
        return __syncol__if$3B179$3B31_50_def0(___x_0, __parse_state_1);
    }
};

function __syncol__if$3B189$3B31_52_def0(___x_0, __typedef_1) {
    return Sapl.feval(__typedef_1);
};

function __syncol__if$3B189$3B31_52(___x_0, __typedef_1) {
    if ((Sapl.feval(___x_0) == true)) {
        return false;
    } else {
        return __syncol__if$3B189$3B31_52_def0(___x_0, __typedef_1);
    }
};

function __syncol__f51_51_def0(___x_0, __indent_1, __line_2) {
    return false;
};

function __syncol__f51_51(___x_0, __indent_1, __line_2) {
    if ((Sapl.feval(___x_0) == true)) {
        if ((!__StdInt_$3C_17(Sapl.feval(__indent_1), 0))) {
            return (!__syncol_whiteChar([___SystemArray_select_19, [__line_2, 0]]));
        } else {
            return false;
        }
    } else {
        return __syncol__f51_51_def0(___x_0, __indent_1, __line_2);
    }
};

function __syncol__f53_53_def0(___x_0, __index_1, __line_size_2) {
    return false;
};

function __syncol__f53_53(___x_0, __index_1, __line_size_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return __StdInt_$3C_17(Sapl.feval(__index_1), Sapl.feval(__line_size_2));
    } else {
        return __syncol__f53_53_def0(___x_0, __index_1, __line_size_2);
    }
};

function __syncol__if$3B193$3B31_54_def0(___x_0, __has_content_1, __line_2, __index_3, __line_size_4, __parse_state_5) {
    return Sapl.feval(__has_content_1);
};

function __syncol__if$3B193$3B31_54(___x_0, __has_content_1, __line_2, __index_3, __line_size_4, __parse_state_5) {
    if ((Sapl.feval(___x_0) == true)) {
        if (Sapl.feval(__has_content_1)) {
            if ((!__StdString_eqstring_9([__StdString_$7C_10, [__line_2, ___predefined__Tuple2(__index_3, [__sub, [
                [__syncol_scanfunny$eval, [__index_3, __line_size_4, __line_2]], 1]])]], "::"))) {
                return __syncol_$3D$3D_31(Sapl.feval(__parse_state_5), __syncol_StartOfBlock());
            } else {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return __syncol__if$3B193$3B31_54_def0(___x_0, __has_content_1, __line_2, __index_3, __line_size_4, __parse_state_5);
    }
};

function __syncol_set_has_content_10(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__a1_1, __a2_1, __a3_1, __a4_1, __a5_1, __val);
};

function __syncol_set_offside_8(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__a1_1, __a2_1, __a3_1, __val, __a5_1, __a6_1);
};

function __syncol_getToken_select0$eval(a0, a1, a2, a3, a4, a5, a6) {
    return __syncol_getToken_select0(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6));
};

function __syncol_getToken_select0(__i_2_0, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((!(__level_0 == 0))) {
        return Sapl.feval(__i_2_0);
    } else {
        return __syncol_scanfunny(Sapl.feval(__i_2_0), __line_size_3, __line_2);
    }
};

function __syncol_getToken_select1$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select1(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select1(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((___SystemArray_select_19(__line_2, __i_1_1) == '/')) {
        var __i_2_0_1 = (Sapl.feval(__i_1_1) + 1);
        return __syncol_getToken_select0(__i_2_0_1, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);;
    } else {
        return __syncol_scanfunny(Sapl.feval(__i_1_1), __line_size_3, __line_2);
    }
};

function __syncol_getToken_select2$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select2(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select2(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_1_1), __line_size_3))) {
        return __line_size_3;
    } else {
        return __syncol_getToken_select1(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select3$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_getToken_select3(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_getToken_select3(__char_2_0, __i_2_1, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_2_0) == '*')) {
        return Sapl.feval(__i_2_1);
    } else {
        return __syncol_scanfunny(Sapl.feval(__i_2_1), __line_size_3, __line_2);
    }
};

function __syncol_getToken_select4$eval(a0, a1, a2, a3, a4, a5, a6, a7) {
    return __syncol_getToken_select4(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5), Sapl.feval(a6), Sapl.feval(a7));
};

function __syncol_getToken_select4(__char_2_0, __i_2_1, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_2_0) == '/')) {
        return Sapl.feval(__i_2_1);
    } else {
        return __syncol_getToken_select3(__char_2_0, __i_2_1, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select5$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select5(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select5(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_1_1), __line_size_3))) {
        return __line_size_3;
    } else {
        var __char_2_0_1 = ___SystemArray_select_19(__line_2, __i_1_1),
            __i_2_1_1 = (Sapl.feval(__i_1_1) + 1);
        return __syncol_getToken_select4(__char_2_0_1, __i_2_1_1, __char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);;
    }
};

function __syncol_getToken_select6$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select6(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select6(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_1_0) == '"')) {
        return (__level_0 == 0);
    } else {
        return false;
    }
};

function __syncol_pS_34_select0(__char_1_0, __line_0, __line_size_1, __i_2) {
    if ((Sapl.feval(__char_1_0) == '\\')) {
        return __syncol_pS_34(__line_0, __line_size_1, [__add, [__i_2, 2]]);
    } else {
        return __syncol_pS_34(__line_0, __line_size_1, [__add, [__i_2, 1]]);
    }
};

function __syncol_pS_34_select1(__char_1_0, __line_0, __line_size_1, __i_2) {
    if ((Sapl.feval(__char_1_0) == '"')) {
        return (Sapl.feval(__i_2) + 1);
    } else {
        return __syncol_pS_34_select0(__char_1_0, __line_0, __line_size_1, __i_2);
    }
};

function __syncol_pS_34(__line_0, __line_size_1, __i_2) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_2), Sapl.feval(__line_size_1)))) {
        return Sapl.feval(__line_size_1);
    } else {
        var __char_1_0_1 = [___SystemArray_select_19, [__line_0, __i_2]];
        return __syncol_pS_34_select1(__char_1_0_1, __line_0, __line_size_1, __i_2);;
    }
};

function __syncol_getToken_select7$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select7(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select7(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_1_0) == '\'')) {
        return (__level_0 == 0);
    } else {
        return false;
    }
};

function __syncol_pC_35_select0(__char_1_0, __line_0, __line_size_1, __i_2) {
    if ((Sapl.feval(__char_1_0) == '\\')) {
        return __syncol_pC_35(__line_0, __line_size_1, [__add, [__i_2, 2]]);
    } else {
        return __syncol_pC_35(__line_0, __line_size_1, [__add, [__i_2, 1]]);
    }
};

function __syncol_pC_35_select1(__char_1_0, __line_0, __line_size_1, __i_2) {
    if ((Sapl.feval(__char_1_0) == '\'')) {
        return (Sapl.feval(__i_2) + 1);
    } else {
        return __syncol_pC_35_select0(__char_1_0, __line_0, __line_size_1, __i_2);
    }
};

function __syncol_pC_35(__line_0, __line_size_1, __i_2) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_2), Sapl.feval(__line_size_1)))) {
        return Sapl.feval(__line_size_1);
    } else {
        var __char_1_0_1 = [___SystemArray_select_19, [__line_0, __i_2]];
        return __syncol_pC_35_select1(__char_1_0_1, __line_0, __line_size_1, __i_2);;
    }
};

function __syncol_getToken_select8$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select8(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select8(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((__level_0 == 0)) {
        return __syncol_funnyChar(__char_1_0);
    } else {
        return false;
    }
};

function __syncol_scanfunny$eval(a0, a1, a2) {
    return __syncol_scanfunny(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __syncol_scanfunny(__i_0, __line_size_1, __line_2) {
    while (1) {
        if ((!__StdInt_$3C_17(__i_0, __line_size_1))) {
            return __line_size_1;
        } else {
            if (__syncol_funnyChar([___SystemArray_select_19, [__line_2, __i_0]])) {
                var t0 = (__i_0 + 1),
                    t1 = __line_size_1,
                    t2 = __line_2;
                __i_0 = t0;
                __line_size_1 = t1;
                __line_2 = t2;
                continue;
            } else {
                return __i_0;
            }
        }
    }
};

function __syncol_getToken_select9$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select9(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select9(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__StdChar_isLower(__char_1_0)) {
        return true;
    } else {
        return __StdChar_isUpper(__char_1_0);
    }
};

function __syncol_cleanChar(__c_0) {
    if (__StdChar_isLower(__c_0)) {
        return true;
    } else {
        if (__StdChar_isUpper(__c_0)) {
            return true;
        } else {
            if (__StdChar_isDigit(__c_0)) {
                return true;
            } else {
                if ((Sapl.feval(__c_0) == '_')) {
                    return true;
                } else {
                    return (Sapl.feval(__c_0) == '`');
                }
            }
        }
    }
};

function __syncol_scanclean$eval(a0, a1, a2) {
    return __syncol_scanclean(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __syncol_scanclean(__i_0, __line_size_1, __line_2) {
    while (1) {
        if ((!__StdInt_$3C_17(__i_0, __line_size_1))) {
            return __line_size_1;
        } else {
            if (__syncol_cleanChar([___SystemArray_select_19, [__line_2, __i_0]])) {
                var t0 = (__i_0 + 1),
                    t1 = __line_size_1,
                    t2 = __line_2;
                __i_0 = t0;
                __line_size_1 = t1;
                __line_2 = t2;
                continue;
            } else {
                return __i_0;
            }
        }
    }
};

function __syncol_scanwhite$eval(a0, a1, a2) {
    return __syncol_scanwhite(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __syncol_scanwhite(__i_0, __line_size_1, __line_2) {
    while (1) {
        if ((!__StdInt_$3C_17(__i_0, __line_size_1))) {
            return __line_size_1;
        } else {
            if (__syncol_whiteChar([___SystemArray_select_19, [__line_2, __i_0]])) {
                var t0 = (__i_0 + 1),
                    t1 = __line_size_1,
                    t2 = __line_2;
                __i_0 = t0;
                __line_size_1 = t1;
                __line_2 = t2;
                continue;
            } else {
                return __i_0;
            }
        }
    }
};

function __syncol_getToken_select10$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select10(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select10(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__syncol_whiteChar(__char_1_0)) {
        return __syncol_scanwhite(Sapl.feval(__i_1_1), __line_size_3, __line_2);
    } else {
        return Sapl.feval(__i_1_1);
    }
};

function __syncol_getToken_select11$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select11(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select11(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__syncol_getToken_select9(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3)) {
        return __syncol_scanclean(Sapl.feval(__i_1_1), __line_size_3, __line_2);
    } else {
        return __syncol_getToken_select10(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select12$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select12(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select12(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__syncol_getToken_select8(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3)) {
        return __syncol_scanfunny(Sapl.feval(__i_1_1), __line_size_3, __line_2);
    } else {
        return __syncol_getToken_select11(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select13$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select13(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select13(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__syncol_getToken_select7(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3)) {
        return __syncol_pC_35(__line_2, __line_size_3, __i_1_1);
    } else {
        return __syncol_getToken_select12(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select14$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select14(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select14(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if (__syncol_getToken_select6(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3)) {
        return __syncol_pS_34(__line_2, __line_size_3, __i_1_1);
    } else {
        return __syncol_getToken_select13(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select15$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select15(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select15(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_1_0) == '/')) {
        return __syncol_getToken_select5(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    } else {
        return __syncol_getToken_select14(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken_select16$eval(a0, a1, a2, a3, a4, a5) {
    return __syncol_getToken_select16(a0, a1, Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4), Sapl.feval(a5));
};

function __syncol_getToken_select16(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3) {
    if ((Sapl.feval(__char_1_0) == '*')) {
        return __syncol_getToken_select2(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    } else {
        return __syncol_getToken_select15(__char_1_0, __i_1_1, __level_0, __index_1, __line_2, __line_size_3);
    }
};

function __syncol_getToken$eval(a0, a1, a2, a3) {
    return __syncol_getToken(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3));
};

function __syncol_getToken(__level_0, __index_1, __line_2, __line_size_3) {
    if ((!__StdInt_$3C_17(__index_1, __line_size_3))) {
        return __line_size_3;
    } else {
        var __char_1_0_1 = ___SystemArray_select_19(__line_2, __index_1),
            __i_1_1_1 = (__index_1 + 1);
        return __syncol_getToken_select16(__char_1_0_1, __i_1_1_1, __level_0, __index_1, __line_2, __line_size_3);;
    }
};

function __syncol_set_level_5(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__val, __a2_1, __a3_1, __a4_1, __a5_1, __a6_1);
};

function __syncol_set_typedef_6(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__a1_1, __val, __a3_1, __a4_1, __a5_1, __a6_1);
};

function __syncol__c$3B221$3B39_43_def0(__parse_state_0, __i_1, __line_2, __line_size_3, __state_4, __end_5) {
    return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [__state_4, __syncol_Other()]], __end_5);
};

function __syncol__c$3B221$3B39_43(__parse_state_0, __i_1, __line_2, __line_size_3, __state_4, __end_5) {
    var ys = Sapl.feval(__parse_state_0);
    switch (ys[0]) {
        case 0:
            if ((Sapl.feval(__i_1) == 0)) {
                return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [
                    [__syncol_set_typedef_6, [__state_4, true]], __syncol_Other()]], __end_5);
            } else {
                return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [__state_4, __syncol_Other()]], __end_5);
            }
        case 1:
            return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [
                [__syncol_set_typedecl_7, [__state_4, true]], __syncol_Other()]], __end_5);
        case 2:
            return __syncol__c$3B221$3B39_43_def0(__parse_state_0, __i_1, __line_2, __line_size_3, __state_4, __end_5);
        case 3:
            return __syncol__c$3B221$3B39_43_def0(__parse_state_0, __i_1, __line_2, __line_size_3, __state_4, __end_5);
        case 4:
            return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [
                [__syncol_set_typedecl_7, [__state_4, true]], __syncol_Other()]], __end_5);
        case 5:
            return __syncol_pL_26(__line_2, __line_size_3, [__syncol_set_parse_state_9, [
                [__syncol_set_typedecl_7, [__state_4, true]], __syncol_Other()]], __end_5);
        case 6:
            return __syncol__c$3B221$3B39_43_def0(__parse_state_0, __i_1, __line_2, __line_size_3, __state_4, __end_5);
    }
};

function __syncol__if$3B220$3B36_44_def0(___x_0, __parse_state_1, __i_2, __line_3, __line_size_4, __state_5, __end_6) {
    return __syncol_pL_26(__line_3, __line_size_4, __state_5, __end_6);
};

function __syncol__if$3B220$3B36_44(___x_0, __parse_state_1, __i_2, __line_3, __line_size_4, __state_5, __end_6) {
    if ((Sapl.feval(___x_0) == true)) {
        return __syncol__c$3B221$3B39_43(__parse_state_1, __i_2, __line_3, __line_size_4, __state_5, __end_6);
    } else {
        return __syncol__if$3B220$3B36_44_def0(___x_0, __parse_state_1, __i_2, __line_3, __line_size_4, __state_5, __end_6);
    }
};

function __syncol_funnyChar(__c_0) {
    return __syncol_isStringMember(Sapl.feval(__c_0), (20 - 1), "~@#$%^?!+-*<>\\/|&=:.");
};

function __syncol__c$3B216$3B11_45_def0_select0(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7) {
    if (__StdChar_isUpper([___SystemArray_select_19, [__line_1, __i_7]])) {
        return true;
    } else {
        return __syncol_funnyChar([___SystemArray_select_19, [__line_1, __i_7]]);
    }
};

function __syncol__c$3B216$3B11_45_def0_select1(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7) {
    if (__StdChar_isLower([___SystemArray_select_19, [__line_1, __i_7]])) {
        return true;
    } else {
        return __syncol__c$3B216$3B11_45_def0_select0(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7);
    }
};

function __syncol_$3D$3D_31_def0$eval(a0, a1) {
    return __syncol_$3D$3D_31_def0(Sapl.feval(a0), Sapl.feval(a1));
};

function __syncol_$3D$3D_31_def0(___x_0, ___x_1) {
    return false;
};

function __syncol_$3D$3D_31$eval(a0, a1) {
    return __syncol_$3D$3D_31(Sapl.feval(a0), Sapl.feval(a1));
};

function __syncol_$3D$3D_31(___x_0, ___x_1) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return true;
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 1:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return true;
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 2:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return true;
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 3:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return true;
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 4:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return true;
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 5:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return true;
                case 6:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
            }
        case 6:
            var ys = ___x_1;
            switch (ys[0]) {
                case 0:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 1:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 2:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 3:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 4:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 5:
                    return __syncol_$3D$3D_31_def0(___x_0, ___x_1);
                case 6:
                    return true;
            }
    }
};

function __syncol_isStringMember_select0$eval(a0, a1, a2, a3) {
    return __syncol_isStringMember_select0(a0, Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3));
};

function __syncol_isStringMember_select0(__c_1_0, __x_0, __i_1, __s_2) {
    if ((Sapl.feval(__c_1_0) == __x_0)) {
        return true;
    } else {
        return __syncol_isStringMember(__x_0, (__i_1 - 1), __s_2);
    }
};

function __syncol_isStringMember$eval(a0, a1, a2) {
    return __syncol_isStringMember(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __syncol_isStringMember(__x_0, __i_1, __s_2) {
    if (__StdInt_$3C_17(__i_1, 0)) {
        return false;
    } else {
        var __c_1_0_1 = ___SystemArray_select_19(__s_2, __i_1);
        return __syncol_isStringMember_select0(__c_1_0_1, __x_0, __i_1, __s_2);;
    }
};

function __syncol_whiteChar(__c_0) {
    return __syncol_isStringMember(Sapl.feval(__c_0), (5 - 1), " \t\f\n\r");
};

function __syncol_set_parse_state_9(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__a1_1, __a2_1, __a3_1, __a4_1, __val, __a6_1);
};

function __syncol__c$3B216$3B11_45_def0(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7) {
    if (__StdChar_isDigit([___SystemArray_select_19, [__line_1, __i_7]])) {
        if ((Sapl.feval(__level_4) == 0)) {
            if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_Fixity())) {
                return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Precedence()]], __end_5);
            } else {
                return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
            }
        } else {
            return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
        }
    } else {
        if (__syncol__c$3B216$3B11_45_def0_select1(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7)) {
            if ((Sapl.feval(__level_4) == 0)) {
                if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_StartOfBlock())) {
                    return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_CleanId()]], __end_5);
                } else {
                    if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_OpenPar())) {
                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_InfixId()]], __end_5);
                    } else {
                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                    }
                }
            } else {
                return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
            }
        } else {
            if (__syncol_whiteChar([___SystemArray_select_19, [__line_1, __i_7]])) {
                return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
            } else {
                if ((Sapl.feval(__level_4) == 0)) {
                    return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                } else {
                    return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                }
            }
        }
    }
};

function __syncol__c$3B216$3B11_45(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7) {
    if (__StdString_eqstring_9(___x_0, "/*")) {
        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_level_5, [__state_3, [__add, [__level_4, 1]]]], __end_5);
    } else {
        if (__StdString_eqstring_9(___x_0, "*/")) {
            return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_level_5, [__state_3, [__sub, [__level_4, 1]]]], __end_5);
        } else {
            if (__StdString_eqstring_9(___x_0, "//")) {
                return Sapl.feval(__state_3);
            } else {
                if (__StdString_eqstring_9(___x_0, "::")) {
                    return __syncol__if$3B220$3B36_44([__eq, [__level_4, 0]], __parse_state_6, __i_7, __line_1, __line_size_2, __state_3, __end_5);
                } else {
                    if (__StdString_eqstring_9(___x_0, "where")) {
                        if ((Sapl.feval(__level_4) == 0)) {
                            return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                        } else {
                            return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                        }
                    } else {
                        if (__StdString_eqstring_9(___x_0, "let")) {
                            if ((Sapl.feval(__level_4) == 0)) {
                                return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                            } else {
                                return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                            }
                        } else {
                            if (__StdString_eqstring_9(___x_0, "infix")) {
                                if ((Sapl.feval(__level_4) == 0)) {
                                    if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_CleanId())) {
                                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Fixity()]], __end_5);
                                    } else {
                                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                                    }
                                } else {
                                    return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                                }
                            } else {
                                if (__StdString_eqstring_9(___x_0, "infixl")) {
                                    if ((Sapl.feval(__level_4) == 0)) {
                                        if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_CleanId())) {
                                            return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Fixity()]], __end_5);
                                        } else {
                                            return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                                        }
                                    } else {
                                        return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                                    }
                                } else {
                                    if (__StdString_eqstring_9(___x_0, "infixr")) {
                                        if ((Sapl.feval(__level_4) == 0)) {
                                            if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_CleanId())) {
                                                return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Fixity()]], __end_5);
                                            } else {
                                                return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                                            }
                                        } else {
                                            return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                                        }
                                    } else {
                                        if (__StdString_eqstring_9(___x_0, "(")) {
                                            if ((Sapl.feval(__level_4) == 0)) {
                                                if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_StartOfBlock())) {
                                                    return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_OpenPar()]], __end_5);
                                                } else {
                                                    return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                                                }
                                            } else {
                                                return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                                            }
                                        } else {
                                            if (__StdString_eqstring_9(___x_0, ")")) {
                                                if ((Sapl.feval(__level_4) == 0)) {
                                                    if (__syncol_$3D$3D_31(Sapl.feval(__parse_state_6), __syncol_InfixId())) {
                                                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_CleanId()]], __end_5);
                                                    } else {
                                                        return __syncol_pL_26(__line_1, __line_size_2, [__syncol_set_parse_state_9, [__state_3, __syncol_Other()]], __end_5);
                                                    }
                                                } else {
                                                    return __syncol_pL_26(__line_1, __line_size_2, __state_3, __end_5);
                                                }
                                            } else {
                                                return __syncol__c$3B216$3B11_45_def0(___x_0, __line_1, __line_size_2, __state_3, __level_4, __end_5, __parse_state_6, __i_7);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
};

function __syncol_pL_26(__line_0, __line_size_1, __state_2, __i_3) {
    var ys = Sapl.feval(__state_2);
    var __level_1_0_1 = ys[2],
        __typedef_1_1_1 = ys[3],
        __typedecl_1_2_1 = ys[4],
        __offside_1_3_1 = ys[5],
        __parse_state_1_4_1 = ys[6],
        __has_content_1_5_1 = ys[7];
    if ((!__StdInt_$3C_17(Sapl.feval(__i_3), Sapl.feval(__line_size_1)))) {
        return Sapl.feval(__state_2);
    } else {
        var __end_2_0_2 = [__syncol_getToken$eval, [__level_1_0_1, __i_3, __line_0, __line_size_1]];
        return __syncol__c$3B216$3B11_45([__StdString_$7C_10, [__line_0, ___predefined__Tuple2(__i_3, [__sub, [__end_2_0_2, 1]])]], __line_0, __line_size_1, __state_2, __level_1_0_1, __end_2_0_2, __parse_state_1_4_1, __i_3);;
    }
};

function __syncol_parseLine(__state_0, __line_1) {
    var ys = Sapl.feval(__state_0);
    var __level_1_0_1 = ys[2],
        __typedef_1_1_1 = ys[3],
        __typedecl_1_2_1 = ys[4],
        __offside_1_3_1 = ys[5],
        __parse_state_1_4_1 = ys[6],
        __has_content_1_5_1 = ys[7];
    var ___x_2_0_2 = __syncol_scanFirst(Sapl.feval(__level_1_0_1), Sapl.feval(__line_1)),
        ___x_2_1_2 = [__syncol__if$3B165$3B31_48, [__typedecl_1_2_1, [function () {
            return Sapl.feval(__index_2_8_2);
        }, []],
            [function () {
                return Sapl.feval(__line_size_2_7_2);
            }, []],
            [function () {
                return Sapl.feval(__indent_2_10_2);
            }, []], __offside_1_3_1, __parse_state_1_4_1]],
        __parse_state_2_2_2 = [__syncol__if$3B179$3B31_50, [
            [__eq, [
                [function () {
                    return Sapl.feval(__indent_2_10_2);
                }, []],
                [function () {
                    return Sapl.feval(__offside$60_2_9_2);
                }, []]
            ]], __parse_state_1_4_1]],
        __typedef_2_3_2 = [__syncol__if$3B189$3B31_52, [
            [__syncol__f51_51, [
                [__eq, [
                    [function () {
                        return Sapl.feval(__index_2_8_2);
                    }, []], 0]],
                [function () {
                    return Sapl.feval(__indent_2_10_2);
                }, []], __line_1]], __typedef_1_1_1]],
        __has_content_2_4_2 = [__syncol__f53_53, [
            [__not, [
                [__StdInt_$3C_17$eval, [
                    [function () {
                        return Sapl.feval(__indent_2_10_2);
                    }, []], 0]]
            ]],
            [function () {
                return Sapl.feval(__index_2_8_2);
            }, []],
            [function () {
                return Sapl.feval(__line_size_2_7_2);
            }, []]
        ]],
        __has_content_2_5_2 = [__syncol__if$3B193$3B31_54, [
            [__StdInt_$3C_17$eval, [0, [function () {
                return Sapl.feval(__index_2_8_2);
            }, []]]], __has_content_2_4_2, __line_1, [function () {
                return Sapl.feval(__index_2_8_2);
            }, []],
            [function () {
                return Sapl.feval(__line_size_2_7_2);
            }, []], __parse_state_2_2_2]],
        __state_2_6_2 = [__syncol_set_has_content_10, [
            [__syncol_set_parse_state_9, [
                [__syncol_set_offside_8, [
                    [__syncol_set_typedecl_7, [
                        [__syncol_set_typedef_6, [
                            [__syncol_set_level_5, [__state_0, [___predefined_tupsels3v2, [___x_2_0_2]]]], __typedef_2_3_2]],
                        [___predefined_tupsels2v0, [___x_2_1_2]]
                    ]],
                    [function () {
                        return Sapl.feval(__offside$60_2_9_2);
                    }, []]
                ]], __parse_state_2_2_2]], __has_content_2_5_2]],
        __line_size_2_7_2 = [___SystemArray_size_20, [__line_1]],
        __index_2_8_2 = [___predefined_tupsels3v0, [___x_2_0_2]],
        __offside$60_2_9_2 = [___predefined_tupsels2v1, [___x_2_1_2]],
        __indent_2_10_2 = [___predefined_tupsels3v1, [___x_2_0_2]];
    return __syncol_pL_26(__line_1, __line_size_2_7_2, __state_2_6_2, __index_2_8_2);;
};

function __syncol_copy(__res_0, __acc_1, __rest_2) {
    return Sapl.feval([__res_0, [
        [__StdList_$2B$2B$eval, [__acc_1, __rest_2]]
    ]]);
};

function __syncol_during_42_select0(__state$60_3_0, ___x_2_0, __line_2_1, ___x_1_0, __lines_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6) {
    if ((!__syncol_get_typedecl_7(__state_3))) {
        return __syncol_get_typedecl_7(__state$60_3_0);
    } else {
        return false;
    }
};

function __syncol_set_typedecl_7(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return __syncol__State(__a1_1, __a2_1, __val, __a4_1, __a5_1, __a6_1);
};

function __syncol_set_is_typedecl_2(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6];
    return __syncol__Info(__a1_1, __a2_1, __val, __a4_1, __a5_1);
};

function __syncol_set_is_typedef_1(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6];
    return __syncol__Info(__a1_1, __val, __a3_1, __a4_1, __a5_1);
};

function __syncol_anon_24(___x_0) {
    var ys = Sapl.feval(___x_0);
    var __info_1_0_1 = ys[2],
        __l_1_1_1 = ys[3];
    return ___predefined__Tuple2([__syncol_set_is_typedecl_2, [
        [__syncol_set_is_typedef_1, [__info_1_0_1, false]], true]], __l_1_1_1);
};

function __syncol_patch(__res_0, __acc_1, __rest_2) {
    return Sapl.feval([__res_0, [
        [__StdList_$2B$2B$eval, [
            [__StdList_map$eval, [__syncol_anon_24, __acc_1]], __rest_2]]
    ]]);
};

function __StdList_$2B$2B_def0$eval(a0, a1) {
    return __StdList_$2B$2B_def0(Sapl.feval(a0), a1);
};

function __StdList_$2B$2B_def0(___x_0, __list_1) {
    return Sapl.feval(__list_1);
};

function __StdList_$2B$2B$eval(a0, a1) {
    return __StdList_$2B$2B(Sapl.feval(a0), a1);
};

function __StdList_$2B$2B(___x_0, __list_1) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var __hd_1_0_1 = ys[2],
                __tl_1_1_1 = ys[3];
            return ___predefined__Cons(__hd_1_0_1, [__StdList_$2B$2B$eval, [__tl_1_1_1, __list_1]]);
        case 1:
            return __StdList_$2B$2B_def0(___x_0, __list_1);
    }
};

function __syncol__Info(__a1, __a2, __a3, __a4, __a5) {
    return [0, 'syncol._Info', __a1, __a2, __a3, __a4, __a5];
};
__syncol__Info.fields = ["syncol.comment_level", "syncol.is_typedef", "syncol.is_typedecl", "syncol.offside_level", "syncol.flush"];

function __syncol_get_level_5(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return Sapl.feval(__a1_1);
};

function __syncol_get_typedef_6(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return Sapl.feval(__a2_1);
};

function __syncol_get_typedecl_7(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return Sapl.feval(__a3_1);
};

function __syncol_get_offside_8(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return Sapl.feval(__a4_1);
};

function __syncol_get_has_content_10(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4],
        __a4_1 = ys[5],
        __a5_1 = ys[6],
        __a6_1 = ys[7];
    return Sapl.feval(__a6_1);
};

function __syncol_during_42_select1(__state$60_3_0, ___x_2_0, __line_2_1, ___x_1_0, __lines_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6) {
    if (__syncol_during_42_select0(__state$60_3_0, ___x_2_0, __line_2_1, ___x_1_0, __lines_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6)) {
        return __syncol_during_42(__end_0, __beg_1, __idx_2, [__syncol_set_typedecl_7, [__state_3, true]], [__syncol_patch, [__res_4, __acc_5]], ___predefined__Nil(), ___x_6);
    } else {
        return __syncol_during_42(__end_0, __beg_1, [__add, [__idx_2, 1]], __state$60_3_0, __res_4, [__StdList_$2B$2B$eval, [__acc_5, ___predefined__Cons(___predefined__Tuple2(__syncol__Info([__syncol_get_level_5, [__state_3]], [__syncol_get_typedef_6, [__state$60_3_0]], [__syncol_get_typedecl_7, [__state$60_3_0]], [__syncol_get_offside_8, [__state$60_3_0]], [__syncol_get_has_content_10, [__state$60_3_0]]), __line_2_1), ___predefined__Nil())]], __lines_1_1);
    }
};

function __syncol_during_42_select2(__state$60_3_0, ___x_2_0, __line_2_1, ___x_1_0, __lines_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6) {
    if (__syncol_get_has_content_10(__state$60_3_0)) {
        return __syncol_during_42(__end_0, __beg_1, [__add, [__idx_2, 1]], __state$60_3_0, [__syncol_copy, [__res_4, __acc_5]], ___predefined__Cons(___predefined__Tuple2(__syncol__Info([__syncol_get_level_5, [__state_3]], [__syncol_get_typedef_6, [__state$60_3_0]], [__syncol_get_typedecl_7, [__state$60_3_0]], [__syncol_get_offside_8, [__state$60_3_0]], [__syncol_get_has_content_10, [__state$60_3_0]]), __line_2_1), ___predefined__Nil()), __lines_1_1);
    } else {
        return __syncol_during_42_select1(__state$60_3_0, ___x_2_0, __line_2_1, ___x_1_0, __lines_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6);
    }
};

function __syncol_during_42(__end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6) {
    var ys = Sapl.feval(___x_6);
    switch (ys[0]) {
        case 0:
            var ___x_1_0_1 = ys[2],
                __lines_1_1_1 = ys[3];
            var ys = Sapl.feval(___x_1_0_1);
            var ___x_2_0_2 = ys[2],
                __line_2_1_2 = ys[3];
            if (__StdInt_$3C_17(Sapl.feval(__end_0), Sapl.feval(__idx_2))) {
                return __syncol_after_41(__beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6);
            } else {
                var __state$60_3_0_3 = [__syncol_parseLine, [__state_3, __line_2_1_2]];
                return __syncol_during_42_select2(__state$60_3_0_3, ___x_2_0_2, __line_2_1_2, ___x_1_0_1, __lines_1_1_1, __end_0, __beg_1, __idx_2, __state_3, __res_4, __acc_5, ___x_6);;
            }
        case 1:
            return ___predefined__Tuple3(__beg_1, [__sub, [__idx_2, 1]], [__res_4, [__acc_5]]);
    }
};

function __StrictList_slAppend$eval(a0, a1) {
    return __StrictList_slAppend(Sapl.feval(a0), Sapl.feval(a1));
};

function __StrictList_slAppend(___x_0, __list$60_1) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var __x_1_0_1 = ys[2],
                __list_1_1_1 = ys[3];
            return __StrictList_SCons(__x_1_0_1, [__StrictList_slAppend$eval, [__list_1_1_1, __list$60_1]]);
        case 1:
            return __list$60_1;
    }
};

function __syncol_before_21(__end_0, __beg_1, __idx_2, __res_3, __state_4, __reversed_acc_5, ___x_6) {
    while (1) {
        var ys = Sapl.feval(___x_6);
        switch (ys[0]) {
            case 0:
                var ___x_1_0_1 = ys[2],
                    __lines_1_1_1 = ys[3];
                var ys = Sapl.feval(___x_1_0_1);
                var __info_2_0_2 = ys[2],
                    __line_2_1_2 = ys[3];
                var ys = Sapl.feval(__info_2_0_2);
                var __level_3_0_3 = ys[2],
                    __def_3_1_3 = ys[3],
                    __dec_3_2_3 = ys[4],
                    __off_3_3_3 = ys[5],
                    __flush_3_4_3 = ys[6];
                if (__StdInt_$3C_17(Sapl.feval(__idx_2), Sapl.feval(__beg_1))) {
                    if (Sapl.feval(__flush_3_4_3)) {
                        var t0 = __end_0,
                            t1 = __beg_1,
                            t2 = [__add, [__idx_2, 1]],
                            t3 = __res_3,
                            t4 = __state_4,
                            t5 = ___predefined__Cons(___predefined__Tuple2(__info_2_0_2, __line_2_1_2), __reversed_acc_5),
                            t6 = __lines_1_1_1;
                        __end_0 = t0;
                        __beg_1 = t1;
                        __idx_2 = t2;
                        __res_3 = t3;
                        __state_4 = t4;
                        __reversed_acc_5 = t5;
                        ___x_6 = t6;
                        continue;
                    } else {
                        var t0 = __end_0,
                            t1 = __beg_1,
                            t2 = [__add, [__idx_2, 1]],
                            t3 = [__syncol_copy, [__res_3, [__StdList_reverse$eval, [__reversed_acc_5]]]],
                            t4 = [__syncol_set_offside_8, [
                                [__syncol_set_typedecl_7, [
                                    [__syncol_set_typedef_6, [
                                        [__syncol_set_level_5, [__state_4, __level_3_0_3]], __def_3_1_3]], __dec_3_2_3]], __off_3_3_3]],
                            t5 = ___predefined__Cons(___predefined__Tuple2(__info_2_0_2, __line_2_1_2), ___predefined__Nil()),
                            t6 = __lines_1_1_1;
                        __end_0 = t0;
                        __beg_1 = t1;
                        __idx_2 = t2;
                        __res_3 = t3;
                        __state_4 = t4;
                        __reversed_acc_5 = t5;
                        ___x_6 = t6;
                        continue;
                    }
                } else {
                    var __beg$60_4_0_4 = [__sub, [__idx_2, [__StdList_length_47$eval, [__reversed_acc_5]]]];
                    return __syncol_during_42(__end_0, __beg$60_4_0_4, __beg$60_4_0_4, __state_4, __res_3, ___predefined__Nil(), [__StrictList_slAppend$eval, [
                        [__StrictList_slFromList$eval, [
                            [__StdList_reverse$eval, [__reversed_acc_5]]
                        ]], ___x_6]]);;
                }
            case 1:
                return ___predefined__Tuple3(0, 0, [__res_3, [
                    [__StdList_reverse$eval, [__reversed_acc_5]]
                ]]);
        }
    }
};

function __StrictList_SCons(__a1, __a2) {
    return [0, 'StrictList.SCons', __a1, __a2];
};

function __StrictList_SNil() {
    return [1, 'StrictList.SNil'];
};

function __StrictList_slFromList$eval(a0) {
    return __StrictList_slFromList(Sapl.feval(a0));
};

function __StrictList_slFromList(___x_0) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            var __x_1_0_1 = ys[2],
                __xs_1_1_1 = ys[3];
            return __StrictList_SCons(__x_1_0_1, [__StrictList_slFromList$eval, [__xs_1_1_1]]);
        case 1:
            return __StrictList_SNil();
    }
};

function __syncol__State(__a1, __a2, __a3, __a4, __a5, __a6) {
    return [0, 'syncol._State', __a1, __a2, __a3, __a4, __a5, __a6];
};
__syncol__State.fields = ["syncol.level", "syncol.typedef", "syncol.typedecl", "syncol.offside", "syncol.parse_state", "syncol.has_content"];

function __syncol_StartOfBlock() {
    return [0, 'syncol.StartOfBlock'];
};

function __syncol_CleanId() {
    return [1, 'syncol.CleanId'];
};

function __syncol_OpenPar() {
    return [2, 'syncol.OpenPar'];
};

function __syncol_InfixId() {
    return [3, 'syncol.InfixId'];
};

function __syncol_Precedence() {
    return [4, 'syncol.Precedence'];
};

function __syncol_Fixity() {
    return [5, 'syncol.Fixity'];
};

function __syncol_Other() {
    return [6, 'syncol.Other'];
};

function __syncol_iniState() {
    return __syncol__State(0, false, false, 0, __syncol_StartOfBlock(), false);
};

function ___predefined__Tuple3(__a1, __a2, __a3) {
    return [0, '_predefined._Tuple3', __a1, __a2, __a3];
};

function ___predefined_tupsels3v0(__t) {
    var ys = Sapl.feval(__t);
    var __a0_1 = ys[2],
        __a1_1 = ys[3],
        __a2_1 = ys[4];
    return Sapl.feval(__a0_1);
};

function ___predefined_tupsels3v1(__t) {
    var ys = Sapl.feval(__t);
    var __a0_1 = ys[2],
        __a1_1 = ys[3],
        __a2_1 = ys[4];
    return Sapl.feval(__a1_1);
};

function ___predefined_tupsels3v2(__t) {
    var ys = Sapl.feval(__t);
    var __a0_1 = ys[2],
        __a1_1 = ys[3],
        __a2_1 = ys[4];
    return Sapl.feval(__a2_1);
};

function __syncol_quickParse$eval(a0, a1, a2) {
    return __syncol_quickParse(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __syncol_quickParse(__beg_0, __end_1, __lines_2) {
    var ___x_1_0_1 = [__syncol_before_21, [__end_1, __beg_0, 0, __StrictList_slFromList$eval, __syncol_iniState, ___predefined__Nil(), __lines_2]];
    return ___predefined__Tuple3([___predefined_tupsels3v0, [___x_1_0_1]], [___predefined_tupsels3v1, [___x_1_0_1]], [___predefined_tupsels3v2, [___x_1_0_1]]);;
};

function ___SystemArray_update_21(__arr_0, __index_1, __el_2) {
    return (Sapl.feval(__arr_0).substr(0, Sapl.feval(__index_1)) + Sapl.feval(__el_2) + Sapl.feval(__arr_0).substr(Sapl.feval(__index_1) + Sapl.feval(__el_2).length));
};

function __Text_copyChars_23$eval(a0, a1, a2, a3, a4) {
    return __Text_copyChars_23(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2), Sapl.feval(a3), Sapl.feval(a4));
};

function __Text_copyChars_23(__offset_0, __i_1, __num_2, __src_3, __dst_4) {
    while (1) {
        if ((__i_1 == __num_2)) {
            return __dst_4;
        } else {
            var t0 = __offset_0,
                t1 = (__i_1 + 1),
                t2 = __num_2,
                t3 = __src_3,
                t4 = ___SystemArray_update_21(__dst_4, [__add, [__offset_0, __i_1]], [___SystemArray_select_19, [__src_3, __i_1]]);
            __offset_0 = t0;
            __i_1 = t1;
            __num_2 = t2;
            __src_3 = t3;
            __dst_4 = t4;
            continue;
        }
    }
};

function __Text_concat$60_22(___x_0, __dst_1, ___x_2) {
    while (1) {
        var ys = Sapl.feval(___x_0);
        switch (ys[0]) {
            case 0:
                var __x_1_0_1 = ys[2],
                    __xs_1_1_1 = ys[3];
                var t0 = __xs_1_1_1,
                    t1 = [__Text_copyChars_23$eval, [___x_2, 0, [___SystemArray_size_20, [__x_1_0_1]], __x_1_0_1, __dst_1]],
                    t2 = [__add, [___x_2, [___SystemArray_size_20, [__x_1_0_1]]]];
                ___x_0 = t0;
                __dst_1 = t1;
                ___x_2 = t2;
                continue;
            case 1:
                return Sapl.feval(__dst_1);
        }
    }
};

function ___SystemArray_createArray_17(__size_0, __el_1) {
    return __string_create(__size_0, __el_1);
};

function __Text_anon_37(__s_0, __a_1) {
    return (Sapl.feval(__s_0) + ___SystemArray_size_20(__a_1));
};

function __Text_foldl_38(__r_0, ___x_1) {
    while (1) {
        var ys = Sapl.feval(___x_1);
        switch (ys[0]) {
            case 0:
                var __a_1_0_1 = ys[2],
                    __x_1_1_1 = ys[3];
                var t0 = [__Text_anon_37, [__r_0, __a_1_0_1]],
                    t1 = __x_1_1_1;
                __r_0 = t0;
                ___x_1 = t1;
                continue;
            case 1:
                return Sapl.feval(__r_0);
        }
    }
};

function __Text_concat_0$eval(a0) {
    return __Text_concat_0(Sapl.feval(a0));
};

function __Text_concat_0(__xs_0) {
    return __Text_concat$60_22(__xs_0, [___SystemArray_createArray_17, [
        [__Text_foldl_38, [0, __xs_0]], '\000']], 0);
};

function __Text_join$60_25_def0$eval(a0, a1, a2, a3) {
    return __Text_join$60_25_def0(a0, a1, Sapl.feval(a2), Sapl.feval(a3));
};

function __Text_join$60_25_def0(__x_0, ___x_1, __sep_2, ___x_3) {
    return ___predefined__Cons(__x_0, ___predefined__Cons(__sep_2, [__Text_join$60_25$eval, [__sep_2, ___x_1]]));
};

function __Text_join$60_25$eval(a0, a1) {
    return __Text_join$60_25(Sapl.feval(a0), Sapl.feval(a1));
};

function __Text_join$60_25(__sep_0, ___x_1) {
    var ys = ___x_1;
    switch (ys[0]) {
        case 0:
            var __x_1_0_1 = ys[2],
                ___x_1_1_1 = ys[3];
            var ys = Sapl.feval(___x_1_1_1);
            switch (ys[0]) {
                case 0:
                    var ___uv1_2_0_2 = ys[2],
                        ___uv2_2_1_2 = ys[3];
                    return __Text_join$60_25_def0(__x_1_0_1, ___x_1_1_1, __sep_0, ___x_1);
                case 1:
                    return ___predefined__Cons(__x_1_0_1, ___predefined__Nil());
            }
        case 1:
            return ___predefined__Nil();
    }
};

function __Text_join_4$eval(a0, a1) {
    return __Text_join_4(Sapl.feval(a0), Sapl.feval(a1));
};

function __Text_join_4(__sep_0, __xs_1) {
    return __Text_concat_0(__Text_join$60_25(__sep_0, __xs_1));
};

function __StdList_map$eval(a0, a1) {
    return __StdList_map(a0, Sapl.feval(a1));
};

function __StdList_map(__f_0, ___x_1) {
    var ys = ___x_1;
    switch (ys[0]) {
        case 0:
            var __a_1_0_1 = ys[2],
                __x_1_1_1 = ys[3];
            return ___predefined__Cons([__f_0, [__a_1_0_1]], [__StdList_map$eval, [__f_0, __x_1_1_1]]);
        case 1:
            return ___predefined__Nil();
    }
};

function __EdTab_toString_32$eval(a0) {
    return __EdTab_toString_32(Sapl.feval(a0));
};

function __EdTab_toString_32(___x_0) {
    var ys = ___x_0;
    switch (ys[0]) {
        case 0:
            return "yellow";
        case 1:
            return "black";
        case 2:
            return "white";
        case 3:
            return Sapl.feval(__nomatch);
        case 4:
            return "grey";
        case 5:
            return Sapl.feval(__nomatch);
        case 6:
            return "red";
        case 7:
            return "green";
        case 8:
            return "blue";
        case 9:
            return Sapl.feval(__nomatch);
        case 10:
            return "magenta";
        case 11:
            return Sapl.feval(__nomatch);
    }
};

function __StdString_$2B$2B$2B_11(__a_0, __b_1) {
    return (Sapl.feval(__a_0) + Sapl.feval(__b_1));
};

function __EdTab_toString_33$eval(a0) {
    return __EdTab_toString_33(Sapl.feval(a0));
};

function __EdTab_toString_33(___x_0) {
    var ys = ___x_0;
    var __s_1_0_1 = ys[2],
        __c_1_1_1 = ys[3];
    return __StdString_$2B$2B$2B_11("<span style=\"color:", [__StdString_$2B$2B$2B_11, [
        [__EdTab_toString_32$eval, [__c_1_1_1]],
        [__StdString_$2B$2B$2B_11, [";\">", [__StdString_$2B$2B$2B_11, [__s_1_0_1, "</span>"]]]]
    ]]);
};

function __StdList_reverse__89$eval(a0, a1) {
    return __StdList_reverse__89(Sapl.feval(a0), a1);
};

function __StdList_reverse__89(___x_0, __list_1) {
    while (1) {
        var ys = ___x_0;
        switch (ys[0]) {
            case 0:
                var __hd_1_0_1 = ys[2],
                    __tl_1_1_1 = ys[3];
                var t0 = Sapl.feval(__tl_1_1_1),
                    t1 = ___predefined__Cons(__hd_1_0_1, __list_1);
                ___x_0 = t0;
                __list_1 = t1;
                continue;
            case 1:
                return Sapl.feval(__list_1);
        }
    }
};

function __StdList_reverse$eval(a0) {
    return __StdList_reverse(Sapl.feval(a0));
};

function __StdList_reverse(__list_0) {
    return __StdList_reverse__89(__list_0, ___predefined__Nil());
};

function __EdTab_asString(__pic_0) {
    var ys = Sapl.feval(__pic_0);
    var __pencolor_1_0_1 = ys[2],
        __len_1_1_1 = ys[3],
        __out_1_2_1 = ys[4];
    return ___predefined__Tuple2([__Text_join_4$eval, ["", [__StdList_map$eval, [__EdTab_toString_33$eval, [__StdList_reverse$eval, [__out_1_2_1]]]]]], __pic_0);
};

function __EdTab_splitAtTabs$60_36(__string_0, __max_1, __start_2, __current_3) {
    while (1) {
        if (__StdInt_$3C_17(Sapl.feval(__max_1), Sapl.feval(__current_3))) {
            return ___predefined__Cons([__StdString_$7C_10, [__string_0, ___predefined__Tuple2(__start_2, __max_1)]], ___predefined__Nil());
        } else {
            if ((___SystemArray_select_19(__string_0, __current_3) == '\t')) {
                var __new_1_0_1 = [__add, [__current_3, 1]];
                return ___predefined__Cons([__StdString_$7C_10, [__string_0, ___predefined__Tuple2(__start_2, [__sub, [__current_3, 1]])]], [__EdTab_splitAtTabs$60_36, [__string_0, __max_1, __new_1_0_1, __new_1_0_1]]);;
            } else {
                var t0 = __string_0,
                    t1 = __max_1,
                    t2 = __start_2,
                    t3 = [__add, [__current_3, 1]];
                __string_0 = t0;
                __max_1 = t1;
                __start_2 = t2;
                __current_3 = t3;
                continue;
            }
        }
    }
};

function __EdTab_splitAtTabs$eval(a0) {
    return __EdTab_splitAtTabs(Sapl.feval(a0));
};

function __EdTab_splitAtTabs(__string_0) {
    return __EdTab_splitAtTabs$60_36(__string_0, [__sub, [
        [___SystemArray_size_20, [__string_0]], 1]], 0, 0);
};

function __EdTab_alignAtTab$60$eval(a0, a1) {
    return __EdTab_alignAtTab$60(Sapl.feval(a0), Sapl.feval(a1));
};

function __EdTab_alignAtTab$60(__tabSize_0, __pic_1) {
    var ys = __pic_1;
    var __pencolor_1_0_1 = ys[2],
        __len_1_1_1 = ys[3],
        __out_1_2_1 = ys[4];
    return __pic_1;
};

function __EdTab_tabSize() {
    return 8;
};

function __EdTab_tabDrawString$60_30_def0$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {
    return __EdTab_tabDrawString$60_30_def0(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, Sapl.feval(a10), Sapl.feval(a11), Sapl.feval(a12));
};

function __EdTab_tabDrawString$60_30_def0(__string_0, ___x_1, __keywordColour_2, __typedeclColour_3, __typedefColour_4, __commentColour_5, __charColour_6, __stringColour_7, __textColour_8, __tabColour_9, ___x_10, ___x_11, __picture_12) {
    var ___x_1_0_1 = __EdTab_drawC_31(__keywordColour_2, __typedeclColour_3, __typedefColour_4, __textColour_8, __commentColour_5, __charColour_6, __stringColour_7, ___x_10, Sapl.feval(__string_0), __picture_12),
        __picture_1_1_1 = __EdTab_alignAtTab$60(__EdTab_tabSize(), ___predefined_tupsels2v1(___x_1_0_1)),
        __picture_1_2_1 = __EdTab_setPenColour(Sapl.feval(__tabColour_9), Sapl.feval(__picture_1_1_1)),
        __picture_1_3_1 = __EdTab_optDrawC('\t', Sapl.feval(__picture_1_2_1)),
        __picture_1_4_1 = __EdTab_setPenColour(Sapl.feval(__textColour_8), Sapl.feval(__picture_1_3_1));
    return __EdTab_tabDrawString$60_30(__keywordColour_2, __typedeclColour_3, __typedefColour_4, __commentColour_5, __charColour_6, __stringColour_7, __textColour_8, __tabColour_9, ___predefined_tupsels2v0(___x_1_0_1), Sapl.feval(___x_1), Sapl.feval(__picture_1_4_1));;
};

function __EdTab__if$3B138$3B22_47_def0(___x_0, __textColour_1, __commentColour_2) {
    return [__EdTab_setPenColour$eval, [__commentColour_2]];
};

function __EdTab__if$3B138$3B22_47(___x_0, __textColour_1, __commentColour_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return [__EdTab_setPenColour$eval, [__textColour_1]];
    } else {
        return __EdTab__if$3B138$3B22_47_def0(___x_0, __textColour_1, __commentColour_2);
    }
};

function __EdTab__if$3B141$3B22_48_def0(___x_0, __typedefColour_1, __commentColour_2) {
    return [__EdTab_setPenColour$eval, [__commentColour_2]];
};

function __EdTab__if$3B141$3B22_48(___x_0, __typedefColour_1, __commentColour_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return [__EdTab_setPenColour$eval, [__typedefColour_1]];
    } else {
        return __EdTab__if$3B141$3B22_48_def0(___x_0, __typedefColour_1, __commentColour_2);
    }
};

function __EdTab_dL_27_select0$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) {
    return __EdTab_dL_27_select0(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, Sapl.feval(a13), Sapl.feval(a14), Sapl.feval(a15));
};

function __EdTab_dL_27_select0(__cl_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__EdTab_in_comment_17(__cl_2_0))) {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_2_0), (Sapl.feval(__i$60_1_0) + 1), __EdTab_setPenColour(__EdTab_non_comment_colour_20(__textColour_0, __typedefColour_1, __typedeclColour_2, __cl_2_0), __EdTab_optDrawS("*/", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13))));
    } else {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_2_0), (Sapl.feval(__i$60_1_0) + 1), __EdTab_optDrawS("*/", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13)));
    }
};

function __EdTab_dL_27_select1$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) {
    return __EdTab_dL_27_select1(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, Sapl.feval(a13), Sapl.feval(a14), Sapl.feval(a15));
};

function __EdTab_dL_27_select1(__i$60$60_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (__StdInt_$3C_17(Sapl.feval(__i$60$60_2_0), Sapl.feval(__l_10))) {
        return __EdTab_funnyChar_37(__s_7, __funnySize_8, __funnyChars_9, __i$60$60_2_0);
    } else {
        return false;
    }
};

function __EdTab__c$3B254$3B30_42(__cl_0) {
    var ys = Sapl.feval(__cl_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2];
            return __EdTab_N([__sub, [__l_1_0_1, 1]]);
        case 1:
            return Sapl.feval(__nomatch);
        case 2:
            return Sapl.feval(__nomatch);
        case 3:
            return Sapl.feval(__nomatch);
        case 4:
            var __l_1_0_1 = ys[2];
            return __EdTab_T([__sub, [__l_1_0_1, 1]]);
        case 5:
            var __l_1_0_1 = ys[2];
            return __EdTab_D([__sub, [__l_1_0_1, 1]]);
    }
};

function __EdTab_dec_comment_18(__cl_0) {
    return __EdTab__c$3B254$3B30_42(__cl_0);
};

function __EdTab_dL_27_select2$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) {
    return __EdTab_dL_27_select2(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, Sapl.feval(a14), Sapl.feval(a15), Sapl.feval(a16));
};

function __EdTab_dL_27_select2(__cl_3_0, __i$60$60_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__EdTab_in_comment_17(__cl_3_0))) {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_3_0), Sapl.feval(__i$60$60_2_0), __EdTab_setPenColour(__EdTab_non_comment_colour_20(__textColour_0, __typedefColour_1, __typedeclColour_2, __cl_3_0), __EdTab_optDrawS("*/", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13))));
    } else {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_3_0), Sapl.feval(__i$60$60_2_0), __EdTab_optDrawS("*/", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13)));
    }
};

function __EdTab_dL_27_select3$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) {
    return __EdTab_dL_27_select3(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, Sapl.feval(a13), Sapl.feval(a14), Sapl.feval(a15));
};

function __EdTab_dL_27_select3(__i$60$60_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (__EdTab_dL_27_select1(__i$60$60_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13)) {
        var __j_3_0_1 = [__EdTab_scanfunny_38, [__s_7, __funnySize_8, __funnyChars_9, __l_10, __i$60$60_2_0]];
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__j_3_0_1), __EdTab_optDrawS(__StdString_$7C_10(__s_7, ___predefined__Tuple2(__i$60$60_2_0, [__sub, [__j_3_0_1, 1]])), __EdTab_optDrawS("*/", __pic_13)));;
    } else {
        var __cl_3_0_1 = [__EdTab_dec_comment_18, [__cl_11]];
        return __EdTab_dL_27_select2(__cl_3_0_1, __i$60$60_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
    }
};

function __EdTab_dL_27_select4$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select4(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select4(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (__EdTab_in_comment_17(__cl_11)) {
        var __cl_2_0_1 = [__EdTab_dec_comment_18, [__cl_11]];
        return __EdTab_dL_27_select0(__cl_2_0_1, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
    } else {
        var __i$60$60_2_0_1 = [__add, [__i$60_1_0, 1]];
        return __EdTab_dL_27_select3(__i$60$60_2_0_1, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
    }
};

function __EdTab_dL_27_select5$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select5(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select5(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((___SystemArray_select_19(__s_7, __i$60_1_0) == '/')) {
        return __EdTab_dL_27_select4(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);
    } else {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__i$60_1_0), __EdTab_optDrawC('*', __pic_13));
    }
};

function __EdTab_dL_27_select6$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select6(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select6(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i$60_1_0), Sapl.feval(__l_10)))) {
        return ___predefined__Tuple2(__cl_11, [__EdTab_optDrawC$eval, ['*', __pic_13]]);
    } else {
        return __EdTab_dL_27_select5(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);
    }
};

function __EdTab__c$3B259$3B30_44(__cl_0) {
    var ys = Sapl.feval(__cl_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2];
            return __EdTab_N([__add, [__l_1_0_1, 1]]);
        case 1:
            return Sapl.feval(__nomatch);
        case 2:
            return Sapl.feval(__nomatch);
        case 3:
            return Sapl.feval(__nomatch);
        case 4:
            var __l_1_0_1 = ys[2];
            return __EdTab_T([__add, [__l_1_0_1, 1]]);
        case 5:
            var __l_1_0_1 = ys[2];
            return __EdTab_D([__add, [__l_1_0_1, 1]]);
    }
};

function __EdTab_inc_comment_19(__cl_0) {
    return __EdTab__c$3B259$3B30_44(__cl_0);
};

function __EdTab__c$3B264$3B37_43(__cl_0, __textColour_1, __typedefColour_2, __typedeclColour_3) {
    var ys = Sapl.feval(__cl_0);
    switch (ys[0]) {
        case 0:
            var ___x_1_0_1 = ys[2];
            return Sapl.feval(__textColour_1);
        case 1:
            return Sapl.feval(__nomatch);
        case 2:
            return Sapl.feval(__nomatch);
        case 3:
            return Sapl.feval(__nomatch);
        case 4:
            var ___x_1_0_1 = ys[2];
            return Sapl.feval(__typedefColour_2);
        case 5:
            var ___x_1_0_1 = ys[2];
            return Sapl.feval(__typedeclColour_3);
    }
};

function __EdTab_non_comment_colour_20(__textColour_0, __typedefColour_1, __typedeclColour_2, __cl_3) {
    return __EdTab__c$3B264$3B37_43(__cl_3, __textColour_0, __typedefColour_1, __typedeclColour_2);
};

function __EdTab_dL_27_select7$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) {
    return __EdTab_dL_27_select7(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, Sapl.feval(a13), Sapl.feval(a14), Sapl.feval(a15));
};

function __EdTab_dL_27_select7(__cl_2_0, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (__EdTab_in_comment_17(__cl_2_0)) {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_2_0), (Sapl.feval(__i$60_1_0) + 1), __EdTab_optDrawS("/*", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13)));
    } else {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, Sapl.feval(__cl_2_0), (Sapl.feval(__i$60_1_0) + 1), __EdTab_setPenColour(__EdTab_non_comment_colour_20(__textColour_0, __typedefColour_1, __typedeclColour_2, __cl_2_0), __EdTab_optDrawS("/*", __EdTab_setPenColour(Sapl.feval(__commentColour_6), __pic_13))));
    }
};

function __EdTab_dL_27_select8$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select8(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select8(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((___SystemArray_select_19(__s_7, __i$60_1_0) == '*')) {
        var __cl_2_0_1 = [__EdTab_inc_comment_19, [__cl_11]];
        return __EdTab_dL_27_select7(__cl_2_0_1, __i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
    } else {
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__i$60_1_0), __EdTab_optDrawC('/', __pic_13));
    }
};

function __EdTab_dL_27_select9$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select9(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select9(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((___SystemArray_select_19(__s_7, __i$60_1_0) == '/')) {
        return ___predefined__Tuple2(__EdTab_L(), [__EdTab_optDrawS$eval, [
            [__StdString_$7C_10, [__s_7, ___predefined__Tuple2([__add, [__i$60_1_0, 1]], __l_10)]],
            [__EdTab_optDrawS$eval, ["//", [__EdTab_setPenColour$eval, [__commentColour_6, __pic_13]]]]
        ]]);
    } else {
        return __EdTab_dL_27_select8(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);
    }
};

function __EdTab_dL_27_select10$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select10(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i$60_1_0), Sapl.feval(__l_10)))) {
        return ___predefined__Tuple2(__cl_11, [__EdTab_optDrawC$eval, ['/', __pic_13]]);
    } else {
        return __EdTab_dL_27_select9(__i$60_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);
    }
};

function __EdTab_dL_27_select11$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dL_27_select11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dL_27_select11(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((___SystemArray_select_19(__s_7, __i_12) == '"')) {
        return (!__EdTab_in_comment_17(__cl_11));
    } else {
        return false;
    }
};

function __EdTab_dS_28_select0$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dS_28_select0(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dS_28_select0(__i_1_0, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_1_0), Sapl.feval(__l_10)))) {
        return ___predefined__Tuple2(__EdTab_S(), [__EdTab_optDrawC$eval, ['\\', __pic_12]]);
    } else {
        return __EdTab_dS_28(__typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, (Sapl.feval(__i_1_0) + 1), __EdTab_optDrawC(___SystemArray_select_19(__s_7, __i_1_0), __EdTab_optDrawC('\\', __pic_12)));
    }
};

function __EdTab_dS_28$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {
    return __EdTab_dS_28(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12));
};

function __EdTab_dS_28(__typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12) {
    while (1) {
        if ((!__StdInt_$3C_17(__i_11, Sapl.feval(__l_10)))) {
            return ___predefined__Tuple2(__EdTab_S(), __pic_12);
        } else {
            if ((___SystemArray_select_19(__s_7, __i_11) == '"')) {
                return __EdTab_dL_27(__textColour_6, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __s_7, __funnySize_8, __funnyChars_9, __l_10, __EdTab_N(0), (__i_11 + 1), __EdTab_setPenColour(Sapl.feval(__textColour_6), __EdTab_optDrawC('"', __pic_12)));
            } else {
                if ((___SystemArray_select_19(__s_7, __i_11) == '\\')) {
                    var __i_1_0_1 = [__add, [__i_11, 1]];
                    return __EdTab_dS_28_select0(__i_1_0_1, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12);;
                } else {
                    var t0 = __typedefColour_0,
                        t1 = __typedeclColour_1,
                        t2 = __keywordColour_2,
                        t3 = __charColour_3,
                        t4 = __stringColour_4,
                        t5 = __commentColour_5,
                        t6 = __textColour_6,
                        t7 = __s_7,
                        t8 = __funnySize_8,
                        t9 = __funnyChars_9,
                        t10 = __l_10,
                        t11 = (__i_11 + 1),
                        t12 = __EdTab_optDrawC(___SystemArray_select_19(__s_7, __i_11), __pic_12);
                    __typedefColour_0 = t0;
                    __typedeclColour_1 = t1;
                    __keywordColour_2 = t2;
                    __charColour_3 = t3;
                    __stringColour_4 = t4;
                    __commentColour_5 = t5;
                    __textColour_6 = t6;
                    __s_7 = t7;
                    __funnySize_8 = t8;
                    __funnyChars_9 = t9;
                    __l_10 = t10;
                    __i_11 = t11;
                    __pic_12 = t12;
                    continue;
                }
            }
        }
    }
};

function __EdTab_dL_27_select12$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dL_27_select12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dL_27_select12(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((___SystemArray_select_19(__s_7, __i_12) == '\'')) {
        return (!__EdTab_in_comment_17(__cl_11));
    } else {
        return false;
    }
};

function __EdTab_dC_29_select0$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dC_29_select0(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dC_29_select0(__i_1_0, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_1_0), Sapl.feval(__l_10)))) {
        return ___predefined__Tuple2(__EdTab_C(), [__EdTab_optDrawC$eval, ['\\', __pic_12]]);
    } else {
        return __EdTab_dC_29(__typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, (Sapl.feval(__i_1_0) + 1), __EdTab_optDrawC(___SystemArray_select_19(__s_7, __i_1_0), __EdTab_optDrawC('\\', __pic_12)));
    }
};

function __EdTab_dC_29$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {
    return __EdTab_dC_29(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12));
};

function __EdTab_dC_29(__typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12) {
    while (1) {
        if ((!__StdInt_$3C_17(__i_11, Sapl.feval(__l_10)))) {
            return ___predefined__Tuple2(__EdTab_C(), __pic_12);
        } else {
            if ((___SystemArray_select_19(__s_7, __i_11) == '\'')) {
                return __EdTab_dL_27(__textColour_6, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __s_7, __funnySize_8, __funnyChars_9, __l_10, __EdTab_N(0), (__i_11 + 1), __EdTab_setPenColour(Sapl.feval(__textColour_6), __EdTab_optDrawC('\'', __pic_12)));
            } else {
                if ((___SystemArray_select_19(__s_7, __i_11) == '\\')) {
                    var __i_1_0_1 = [__add, [__i_11, 1]];
                    return __EdTab_dC_29_select0(__i_1_0_1, __typedefColour_0, __typedeclColour_1, __keywordColour_2, __charColour_3, __stringColour_4, __commentColour_5, __textColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __i_11, __pic_12);;
                } else {
                    var t0 = __typedefColour_0,
                        t1 = __typedeclColour_1,
                        t2 = __keywordColour_2,
                        t3 = __charColour_3,
                        t4 = __stringColour_4,
                        t5 = __commentColour_5,
                        t6 = __textColour_6,
                        t7 = __s_7,
                        t8 = __funnySize_8,
                        t9 = __funnyChars_9,
                        t10 = __l_10,
                        t11 = (__i_11 + 1),
                        t12 = __EdTab_optDrawC(___SystemArray_select_19(__s_7, __i_11), __pic_12);
                    __typedefColour_0 = t0;
                    __typedeclColour_1 = t1;
                    __keywordColour_2 = t2;
                    __charColour_3 = t3;
                    __stringColour_4 = t4;
                    __commentColour_5 = t5;
                    __textColour_6 = t6;
                    __s_7 = t7;
                    __funnySize_8 = t8;
                    __funnyChars_9 = t9;
                    __l_10 = t10;
                    __i_11 = t11;
                    __pic_12 = t12;
                    continue;
                }
            }
        }
    }
};

function __StdString_toString_6(__a_0) {
    return (___string_create(1).substr(0, 0) + Sapl.feval(__a_0) + ___string_create(1).substr(0 + Sapl.feval(__a_0).length));
};

function __EdTab_optDrawC$eval(a0, a1) {
    return __EdTab_optDrawC(Sapl.feval(a0), Sapl.feval(a1));
};

function __EdTab_optDrawC(__c_0, __pic_1) {
    return __EdTab_set_out_15([__EdTab_set_len_14, [__pic_1, [__add, [
        [__EdTab_get_len_14, [__pic_1]], 1]]]], ___predefined__Cons(___predefined__Tuple2([__StdString_toString_6, [__c_0]], [__EdTab_get_pencolor_13, [__pic_1]]), [__EdTab_get_out_15, [__pic_1]]));
};

function __EdTab_dL_27_select13$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dL_27_select13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dL_27_select13(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__EdTab_in_comment_17(__cl_11))) {
        return __EdTab_funnyChar_37(__s_7, __funnySize_8, __funnyChars_9, __i_12);
    } else {
        return false;
    }
};

function __EdTab_isStringMember_26$eval(a0, a1, a2) {
    return __EdTab_isStringMember_26(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __EdTab_isStringMember_26(__x_0, __i_1, __s_2) {
    while (1) {
        if (__StdInt_$3C_17(__i_1, 0)) {
            return false;
        } else {
            if ((___SystemArray_select_19(__s_2, __i_1) == __x_0)) {
                return true;
            } else {
                var t0 = __x_0,
                    t1 = (__i_1 - 1),
                    t2 = __s_2;
                __x_0 = t0;
                __i_1 = t1;
                __s_2 = t2;
                continue;
            }
        }
    }
};

function __EdTab_funnyChar_37(__s_0, __funnySize_1, __funnyChars_2, __i_3) {
    return __EdTab_isStringMember_26(___SystemArray_select_19(__s_0, __i_3), (Sapl.feval(__funnySize_1) - 1), Sapl.feval(__funnyChars_2));
};

function __EdTab_scanfunny_38(__s_0, __funnySize_1, __funnyChars_2, __l_3, __i_4) {
    while (1) {
        if ((!__StdInt_$3C_17(Sapl.feval(__i_4), Sapl.feval(__l_3)))) {
            return Sapl.feval(__l_3);
        } else {
            if (__EdTab_funnyChar_37(__s_0, __funnySize_1, __funnyChars_2, __i_4)) {
                var t0 = __s_0,
                    t1 = __funnySize_1,
                    t2 = __funnyChars_2,
                    t3 = __l_3,
                    t4 = [__add, [__i_4, 1]];
                __s_0 = t0;
                __funnySize_1 = t1;
                __funnyChars_2 = t2;
                __l_3 = t3;
                __i_4 = t4;
                continue;
            } else {
                return Sapl.feval(__i_4);
            }
        }
    }
};

function __EdTab__f46_46_def0(___x_0, __c_1) {
    return (Sapl.feval(__c_1) == '_');
};

function __EdTab__f46_46(___x_0, __c_1) {
    if ((Sapl.feval(___x_0) == true)) {
        return true;
    } else {
        return __EdTab__f46_46_def0(___x_0, __c_1);
    }
};

function __EdTab_scankeyword_39_select0$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select0(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select0(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 4))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 3]])]], "from");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select1$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select1(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select1(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select0(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select2$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select2(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select2(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 7))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 6]])]], "generic");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select3$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select3(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select3(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select2(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select4$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select4(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select4(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 7))) {
        return ___predefined__Tuple2([__StdString_eqstring_9, [
            [__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 6]])]], "dynamic"]], __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select5$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select5(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select5(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return ___predefined__Tuple2([__StdString_eqstring_9, [
            [__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "derive"]], __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select4(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select6$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select6(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select6(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 10))) {
        return ___predefined__Tuple2([__StdString_eqstring_9, [
            [__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 9]])]], "definition"]], __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select5(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select7$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select7(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select7(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 14))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 13]])]], "implementation");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select8$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select8(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select8(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 8))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 7]])]], "instance");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select9$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select9(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select9(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "import");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select10$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select10(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select10(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "infixl");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select11$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select11(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select11(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "infixr");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select12$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select12(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select12(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 5))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 4]])]], "infix");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select13$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select13(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select13(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 2))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 1]])]], "if");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select14$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select14(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select14(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 2))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 1]])]], "in");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select15$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select15(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select15(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select14(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select16$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select16(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select16(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select13(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select15(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select17$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select17(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select17(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select12(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select16(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select18$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select18(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select18(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select11(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select17(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select19$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select19(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select19(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select10(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select18(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select20$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select20(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select20(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select9(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select19(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select21$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select21(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select21(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select8(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select20(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select22$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select22(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select22(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select7(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select21(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select23$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select23(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select23(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "export");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select24$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select24(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select24(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select23(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select25$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select25(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select25(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "module");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select26$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select26(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select26(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select25(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select27$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select27(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select27(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 6))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 5]])]], "system");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select28$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select28(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select28(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select27(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select29$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select29(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select29(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 5))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 4]])]], "class");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select30$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select30(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select30(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 3]])]], "code")) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select31$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select31(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select31(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 3]])]], "case")) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select30(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select32$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select32(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select32(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 4))) {
        return __EdTab_scankeyword_39_select31(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select33$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select33(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select33(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select29(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select32(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select34$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select34(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select34(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 4))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 3]])]], "let!");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select35$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select35(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select35(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 3))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 2]])]], "let");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select36$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select36(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select36(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select35(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select37$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select37(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select37(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select34(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select36(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select38$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select38(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select38(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 2))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 1]])]], "of");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select39$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select39(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select39(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select38(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select40$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select40(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select40(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 4))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 3]])]], "with");
    } else {
        return false;
    }
};

function __StdString_eqs_10(__a_0, __b_1, __ind_2, __size_3) {
    while (1) {
        if ((Sapl.feval(__size_3) > Sapl.feval(__ind_2))) {
            if (((Sapl.feval(__a_0).charAt(Sapl.feval(__ind_2))) == (Sapl.feval(__b_1).charAt(Sapl.feval(__ind_2))))) {
                var t0 = __a_0,
                    t1 = __b_1,
                    t2 = [__add, [__ind_2, 1]],
                    t3 = __size_3;
                __a_0 = t0;
                __b_1 = t1;
                __ind_2 = t2;
                __size_3 = t3;
                continue;
            } else {
                return false;
            }
        } else {
            return true;
        }
    }
};

function __StdString_eqstring_9(__a_0, __b_1) {
    if (((Sapl.feval(__a_0).length) == (Sapl.feval(__b_1).length))) {
        return __StdString_eqs_10(__a_0, __b_1, 0, [__strlen, [__a_0]]);
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select41$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select41(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select41(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__j_2_0) == (__i_3 + 5))) {
        return __StdString_eqstring_9([__StdString_$7C_10, [__s_2, ___predefined__Tuple2(__i_3, [__add, [__i_3, 4]])]], "where");
    } else {
        return false;
    }
};

function __EdTab_scankeyword_39_select42$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select42(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select42(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select41(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return ___predefined__Tuple2(false, __j_2_0);
    }
};

function __EdTab_scankeyword_39_select43$eval(a0, a1, a2, a3, a4, a5) {
    return __EdTab_scankeyword_39_select43(a0, a1, a2, a3, Sapl.feval(a4), Sapl.feval(a5));
};

function __EdTab_scankeyword_39_select43(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if (__EdTab_scankeyword_39_select40(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3)) {
        return ___predefined__Tuple2(true, __j_2_0);
    } else {
        return __EdTab_scankeyword_39_select42(__j_2_0, __c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __StdInt_$3C_17$eval(a0, a1) {
    return __StdInt_$3C_17(Sapl.feval(a0), Sapl.feval(a1));
};

function __StdInt_$3C_17(__a_0, __b_1) {
    return (__b_1 > __a_0);
};

function ___SystemArray_select_19(__arr_0, __index_1) {
    return (Sapl.feval(__arr_0).charAt(Sapl.feval(__index_1)));
};

function __StdBool_$2F$2F$eval(a0, a1) {
    return __StdBool_$2F$2F(Sapl.feval(a0), Sapl.feval(a1));
};

function __StdBool_$2F$2F(__a_0, __b_1) {
    if (__a_0) {
        return true;
    } else {
        return __b_1;
    }
};

function __StdChar_isLower(__c_0) {
    if ((Sapl.feval(__c_0) >= 'a')) {
        if (('z' >= Sapl.feval(__c_0))) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
};

function __StdChar_isUpper(__c_0) {
    if ((Sapl.feval(__c_0) >= 'A')) {
        if (('Z' >= Sapl.feval(__c_0))) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
};

function __StdChar_isAlpha(__c_0) {
    return __StdBool_$2F$2F(__StdChar_isLower(__c_0), __StdChar_isUpper(__c_0));
};

function __StdChar_isDigit(__c_0) {
    if ((Sapl.feval(__c_0) >= '0')) {
        if (('9' >= Sapl.feval(__c_0))) {
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
};

function __StdChar_isAlphanum(__c_0) {
    return __StdBool_$2F$2F(__StdChar_isAlpha(__c_0), __StdChar_isDigit(__c_0));
};

function __EdTab_scanalpha_40_select0(__c_1_0, __s_0, __l_1, __i_2) {
    if ((Sapl.feval(__c_1_0) == '`')) {
        return __EdTab_scanalpha_40(__s_0, __l_1, [__add, [__i_2, 1]]);
    } else {
        return Sapl.feval(__i_2);
    }
};

function __EdTab_scanalpha_40_select1(__c_1_0, __s_0, __l_1, __i_2) {
    if ((Sapl.feval(__c_1_0) == '_')) {
        return __EdTab_scanalpha_40(__s_0, __l_1, [__add, [__i_2, 1]]);
    } else {
        return __EdTab_scanalpha_40_select0(__c_1_0, __s_0, __l_1, __i_2);
    }
};

function __EdTab_scanalpha_40_select2(__c_1_0, __s_0, __l_1, __i_2) {
    if (__StdChar_isAlphanum(__c_1_0)) {
        return __EdTab_scanalpha_40(__s_0, __l_1, [__add, [__i_2, 1]]);
    } else {
        return __EdTab_scanalpha_40_select1(__c_1_0, __s_0, __l_1, __i_2);
    }
};

function __EdTab_scanalpha_40(__s_0, __l_1, __i_2) {
    if ((!__StdInt_$3C_17(Sapl.feval(__i_2), Sapl.feval(__l_1)))) {
        return Sapl.feval(__l_1);
    } else {
        var __c_1_0_1 = [___SystemArray_select_19, [__s_0, __i_2]];
        return __EdTab_scanalpha_40_select2(__c_1_0_1, __s_0, __l_1, __i_2);;
    }
};

function __EdTab_scankeyword_39_select44$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select44(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select44(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'w')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select43(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return ___predefined__Tuple2(false, [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]]);
    }
};

function __EdTab_scankeyword_39_select45$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select45(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select45(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'o')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select39(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select44(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select46$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select46(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select46(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'l')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select37(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select45(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select47$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select47(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select47(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'c')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select33(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select46(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select48$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select48(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select48(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 's')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select28(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select47(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select49$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select49(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select49(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'm')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select26(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select48(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select50$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select50(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select50(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'e')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select24(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select49(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select51$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select51(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select51(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'i')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select22(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select50(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select52$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select52(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select52(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'd')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select6(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select51(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select53$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select53(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select53(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'g')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select3(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select52(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select54$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select54(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select54(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((Sapl.feval(__c_1_0) == 'f')) {
        var __j_2_0_1 = [__EdTab_scanalpha_40, [__s_0, __l_1, [__add, [__i_3, 1]]]];
        return __EdTab_scankeyword_39_select1(__j_2_0_1, __c_1_0, __s_0, __l_1, __s_2, __i_3);;
    } else {
        return __EdTab_scankeyword_39_select53(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39_select55$eval(a0, a1, a2, a3, a4) {
    return __EdTab_scankeyword_39_select55(a0, a1, a2, Sapl.feval(a3), Sapl.feval(a4));
};

function __EdTab_scankeyword_39_select55(__c_1_0, __s_0, __l_1, __s_2, __i_3) {
    if ((!__EdTab__f46_46([__StdChar_isAlpha, [__c_1_0]], __c_1_0))) {
        return ___predefined__Tuple2(false, [__add, [__i_3, 1]]);
    } else {
        return __EdTab_scankeyword_39_select54(__c_1_0, __s_0, __l_1, __s_2, __i_3);
    }
};

function __EdTab_scankeyword_39$eval(a0, a1, a2, a3) {
    return __EdTab_scankeyword_39(a0, a1, Sapl.feval(a2), Sapl.feval(a3));
};

function __EdTab_scankeyword_39(__s_0, __l_1, __s_2, __i_3) {
    var __c_1_0_1 = [___SystemArray_select_19, [__s_2, __i_3]];
    return __EdTab_scankeyword_39_select55(__c_1_0_1, __s_0, __l_1, __s_2, __i_3);;
};

function __EdTab__c$3B248$3B29_41_def0(__cl_0) {
    return false;
};

function __EdTab__c$3B248$3B29_41(__cl_0) {
    var ys = Sapl.feval(__cl_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2];
            return (!(Sapl.feval(__l_1_0_1) == 0));
        case 1:
            return __EdTab__c$3B248$3B29_41_def0(__cl_0);
        case 2:
            return __EdTab__c$3B248$3B29_41_def0(__cl_0);
        case 3:
            return __EdTab__c$3B248$3B29_41_def0(__cl_0);
        case 4:
            var __l_1_0_1 = ys[2];
            return (!(Sapl.feval(__l_1_0_1) == 0));
        case 5:
            var __l_1_0_1 = ys[2];
            return (!(Sapl.feval(__l_1_0_1) == 0));
    }
};

function __EdTab_in_comment_17(__cl_0) {
    return __EdTab__c$3B248$3B29_41(__cl_0);
};

function __EdTab_dL_27_select14$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select14(___x_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (___predefined_tupsels2v0(___x_1_0)) {
        return (!__EdTab_in_comment_17(__cl_11));
    } else {
        return false;
    }
};

function __EdTab_getPenColour$eval(a0) {
    return __EdTab_getPenColour(Sapl.feval(a0));
};

function __EdTab_getPenColour(__pic_0) {
    var ys = __pic_0;
    var __pencolor_1_0_1 = ys[2],
        __len_1_1_1 = ys[3],
        __out_1_2_1 = ys[4];
    return ___predefined__Tuple2(__pencolor_1_0_1, __pic_0);
};

function ___predefined_tupsels2v0(__t) {
    var ys = Sapl.feval(__t);
    var __a0_1 = ys[2],
        __a1_1 = ys[3];
    return Sapl.feval(__a0_1);
};

function __EdTab_set_out_15(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return __EdTab__Picture(__a1_1, __a2_1, __val);
};

function __EdTab_set_len_14(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return __EdTab__Picture(__a1_1, __val, __a3_1);
};

function __EdTab_get_len_14(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return Sapl.feval(__a2_1);
};

function __EdTab_get_pencolor_13(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return Sapl.feval(__a1_1);
};

function __EdTab_get_out_15(__rec) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return Sapl.feval(__a3_1);
};

function __EdTab_optDrawS$eval(a0, a1) {
    return __EdTab_optDrawS(Sapl.feval(a0), Sapl.feval(a1));
};

function __EdTab_optDrawS(__s_0, __pic_1) {
    return __EdTab_set_out_15([__EdTab_set_len_14, [__pic_1, [__add, [
        [__EdTab_get_len_14, [__pic_1]],
        [___SystemArray_size_20, [__s_0]]
    ]]]], ___predefined__Cons(___predefined__Tuple2(__s_0, [__EdTab_get_pencolor_13, [__pic_1]]), [__EdTab_get_out_15, [__pic_1]]));
};

function __StdString_$7C_10(__str_0, ___x_1) {
    var ys = Sapl.feval(___x_1);
    var __a_1 = ys[2],
        __b_1 = ys[3];
    return (Sapl.feval(__str_0).substr(Sapl.feval(__a_1), (Sapl.feval(__b_1) - Sapl.feval(__a_1) + 1)));
};

function ___predefined__Tuple2(__a1, __a2) {
    return [0, '_predefined._Tuple2', __a1, __a2];
};

function __EdTab_dL_27_select15$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
    return __EdTab_dL_27_select15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, Sapl.feval(a12), Sapl.feval(a13), Sapl.feval(a14));
};

function __EdTab_dL_27_select15(___x_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if (__EdTab_dL_27_select14(___x_1_0, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13)) {
        var __j_2_0_1 = [___predefined_tupsels2v1, [___x_1_0]],
            ___x_2_1_1 = [__EdTab_getPenColour$eval, [__pic_13]];
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__j_2_0_1), __EdTab_setPenColour(___predefined_tupsels2v0(___x_2_1_1), __EdTab_optDrawS(__StdString_$7C_10(__s_7, ___predefined__Tuple2(__i_12, [__sub, [__j_2_0_1, 1]])), __EdTab_setPenColour(Sapl.feval(__keywordColour_3), ___predefined_tupsels2v1(___x_2_1_1)))));;
    } else {
        var __j_2_0_1 = [___predefined_tupsels2v1, [___x_1_0]];
        return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__j_2_0_1), __EdTab_optDrawS(__StdString_$7C_10(__s_7, ___predefined__Tuple2(__i_12, [__sub, [__j_2_0_1, 1]])), __pic_13));;
    }
};

function __EdTab_dL_27$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) {
    return __EdTab_dL_27(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12), Sapl.feval(a13));
};

function __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13) {
    if ((!__StdInt_$3C_17(__i_12, Sapl.feval(__l_10)))) {
        return ___predefined__Tuple2(__cl_11, __pic_13);
    } else {
        if ((___SystemArray_select_19(__s_7, __i_12) == '*')) {
            var __i$60_1_0_1 = [__add, [__i_12, 1]];
            return __EdTab_dL_27_select6(__i$60_1_0_1, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
        } else {
            if ((___SystemArray_select_19(__s_7, __i_12) == '/')) {
                var __i$60_1_0_1 = [__add, [__i_12, 1]];
                return __EdTab_dL_27_select10(__i$60_1_0_1, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
            } else {
                if (__EdTab_dL_27_select11(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13)) {
                    return __EdTab_dS_28(__typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __textColour_0, __s_7, __funnySize_8, __funnyChars_9, __l_10, (__i_12 + 1), __EdTab_optDrawC('"', __EdTab_setPenColour(Sapl.feval(__stringColour_5), __pic_13)));
                } else {
                    if (__EdTab_dL_27_select12(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13)) {
                        return __EdTab_dC_29(__typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __textColour_0, __s_7, __funnySize_8, __funnyChars_9, __l_10, (__i_12 + 1), __EdTab_optDrawC('\'', __EdTab_setPenColour(Sapl.feval(__charColour_4), __pic_13)));
                    } else {
                        if (__EdTab_dL_27_select13(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13)) {
                            var __j_1_0_1 = [__EdTab_scanfunny_38, [__s_7, __funnySize_8, __funnyChars_9, __l_10, __i_12]];
                            return __EdTab_dL_27(__textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, Sapl.feval(__j_1_0_1), __EdTab_optDrawS(__StdString_$7C_10(__s_7, ___predefined__Tuple2(__i_12, [__sub, [__j_1_0_1, 1]])), __pic_13));;
                        } else {
                            var ___x_1_0_1 = [__EdTab_scankeyword_39$eval, [__s_7, __l_10, __s_7, __i_12]];
                            return __EdTab_dL_27_select15(___x_1_0_1, __textColour_0, __typedefColour_1, __typedeclColour_2, __keywordColour_3, __charColour_4, __stringColour_5, __commentColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, __cl_11, __i_12, __pic_13);;
                        }
                    }
                }
            }
        }
    }
};

function __EdTab_set_pencolor_13(__rec, __val) {
    var ys = Sapl.feval(__rec);
    var __a1_1 = ys[2],
        __a2_1 = ys[3],
        __a3_1 = ys[4];
    return __EdTab__Picture(__val, __a2_1, __a3_1);
};

function __EdTab_setPenColour$eval(a0, a1) {
    return __EdTab_setPenColour(Sapl.feval(a0), Sapl.feval(a1));
};

function __EdTab_setPenColour(__c_0, __pic_1) {
    return __EdTab_set_pencolor_13(__pic_1, __c_0);
};

function __EdTab__if$3B144$3B22_49_def0(___x_0, __typedeclColour_1, __commentColour_2) {
    return [__EdTab_setPenColour$eval, [__commentColour_2]];
};

function __EdTab__if$3B144$3B22_49(___x_0, __typedeclColour_1, __commentColour_2) {
    if ((Sapl.feval(___x_0) == true)) {
        return [__EdTab_setPenColour$eval, [__typedeclColour_1]];
    } else {
        return __EdTab__if$3B144$3B22_49_def0(___x_0, __typedeclColour_1, __commentColour_2);
    }
};

function __EdTab_drawC_24$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) {
    return __EdTab_drawC_24(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, Sapl.feval(a11), Sapl.feval(a12));
};

function __EdTab_drawC_24(__keywordColour_0, __typedeclColour_1, __typedefColour_2, __textColour_3, __commentColour_4, __charColour_5, __stringColour_6, __s_7, __funnySize_8, __funnyChars_9, __l_10, ___x_11, __pic_12) {
    var ys = ___x_11;
    switch (ys[0]) {
        case 0:
            var __cl_1_0_1 = ys[2];
            return __EdTab_dL_27(__textColour_3, __typedefColour_2, __typedeclColour_1, __keywordColour_0, __charColour_5, __stringColour_6, __commentColour_4, __s_7, __funnySize_8, __funnyChars_9, __l_10, __EdTab_N(__cl_1_0_1), 0, Sapl.feval([__EdTab__if$3B138$3B22_47, [
                [__eq, [__cl_1_0_1, 0]], __textColour_3, __commentColour_4, __pic_12]]));
        case 1:
            return ___predefined__Tuple2(__EdTab_L(), [__EdTab_optDrawS$eval, [__s_7, [__EdTab_setPenColour$eval, [__commentColour_4, __pic_12]]]]);
        case 2:
            return __EdTab_dS_28(__typedefColour_2, __typedeclColour_1, __keywordColour_0, __charColour_5, __stringColour_6, __commentColour_4, __textColour_3, __s_7, __funnySize_8, __funnyChars_9, __l_10, 0, __EdTab_setPenColour(Sapl.feval(__stringColour_6), __pic_12));
        case 3:
            return __EdTab_dC_29(__typedefColour_2, __typedeclColour_1, __keywordColour_0, __charColour_5, __stringColour_6, __commentColour_4, __textColour_3, __s_7, __funnySize_8, __funnyChars_9, __l_10, 0, __EdTab_setPenColour(Sapl.feval(__charColour_5), __pic_12));
        case 4:
            var __cl_1_0_1 = ys[2];
            return __EdTab_dL_27(__textColour_3, __typedefColour_2, __typedeclColour_1, __keywordColour_0, __charColour_5, __stringColour_6, __commentColour_4, __s_7, __funnySize_8, __funnyChars_9, __l_10, __EdTab_T(__cl_1_0_1), 0, Sapl.feval([__EdTab__if$3B141$3B22_48, [
                [__eq, [__cl_1_0_1, 0]], __typedefColour_2, __commentColour_4, __pic_12]]));
        case 5:
            var __cl_1_0_1 = ys[2];
            return __EdTab_dL_27(__textColour_3, __typedefColour_2, __typedeclColour_1, __keywordColour_0, __charColour_5, __stringColour_6, __commentColour_4, __s_7, __funnySize_8, __funnyChars_9, __l_10, __EdTab_D(__cl_1_0_1), 0, Sapl.feval([__EdTab__if$3B144$3B22_49, [
                [__eq, [__cl_1_0_1, 0]], __typedeclColour_1, __commentColour_4, __pic_12]]));
    }
};

function ___SystemArray_size_20(__arr_0) {
    return (Sapl.feval(__arr_0).length);
};

function __EdTab_drawC_31$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) {
    return __EdTab_drawC_31(a0, a1, a2, a3, a4, a5, a6, Sapl.feval(a7), Sapl.feval(a8), Sapl.feval(a9));
};

function __EdTab_drawC_31(__keywordColour_0, __typedeclColour_1, __typedefColour_2, __textColour_3, __commentColour_4, __charColour_5, __stringColour_6, __c_7, __s_8, __pic_9) {
    return __EdTab_drawC_24(__keywordColour_0, __typedeclColour_1, __typedefColour_2, __textColour_3, __commentColour_4, __charColour_5, __stringColour_6, __s_8, 20, "~@#$%^?!+-*<>\\/|&=:.", [___SystemArray_size_20, [__s_8]], __c_7, __pic_9);
};

function ___predefined_tupsels2v1(__t) {
    var ys = Sapl.feval(__t);
    var __a0_1 = ys[2],
        __a1_1 = ys[3];
    return Sapl.feval(__a1_1);
};

function __EdTab_tabDrawString$60_30$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) {
    return __EdTab_tabDrawString$60_30(a0, a1, a2, a3, a4, a5, a6, a7, Sapl.feval(a8), Sapl.feval(a9), Sapl.feval(a10));
};

function __EdTab_tabDrawString$60_30(__keywordColour_0, __typedeclColour_1, __typedefColour_2, __commentColour_3, __charColour_4, __stringColour_5, __textColour_6, __tabColour_7, ___x_8, ___x_9, __picture_10) {
    var ys = ___x_9;
    switch (ys[0]) {
        case 0:
            var __string_1_0_1 = ys[2],
                ___x_1_1_1 = ys[3];
            var ys = Sapl.feval(___x_1_1_1);
            switch (ys[0]) {
                case 0:
                    var ___uv1_2_0_2 = ys[2],
                        ___uv2_2_1_2 = ys[3];
                    return __EdTab_tabDrawString$60_30_def0(__string_1_0_1, ___x_1_1_1, __keywordColour_0, __typedeclColour_1, __typedefColour_2, __commentColour_3, __charColour_4, __stringColour_5, __textColour_6, __tabColour_7, ___x_8, ___x_9, __picture_10);
                case 1:
                    var ___x_3_0_2 = __EdTab_drawC_31(__keywordColour_0, __typedeclColour_1, __typedefColour_2, __textColour_6, __commentColour_3, __charColour_4, __stringColour_5, ___x_8, Sapl.feval(__string_1_0_1), __picture_10);
                    return ___predefined_tupsels2v1(___x_3_0_2);;
            }
        case 1:
            return __picture_10;
    }
};

function __EdTab_N(__a1) {
    return [0, 'EdTab.N', __a1];
};

function __EdTab_L() {
    return [1, 'EdTab.L'];
};

function __EdTab_S() {
    return [2, 'EdTab.S'];
};

function __EdTab_C() {
    return [3, 'EdTab.C'];
};

function __EdTab_T(__a1) {
    return [4, 'EdTab.T', __a1];
};

function __EdTab_D(__a1) {
    return [5, 'EdTab.D', __a1];
};

function __EdTab_tabDrawStringC_select0$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) {
    return __EdTab_tabDrawStringC_select0(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, Sapl.feval(a18), Sapl.feval(a19), Sapl.feval(a20));
};

function __EdTab_tabDrawStringC_select0(__strings_4_0, __textColour_3_0, __backgroundColour_3_1, __marginColour_3_2, __tabColour_3_3, __commentColour_3_4, __stringColour_3_5, __charColour_3_6, __keywordColour_3_7, __typedefColour_3_8, __typedeclColour_3_9, __clevel_2_0, __typedef_2_1, __typedecl_2_2, __offside_level_2_3, __flush_2_4, ___x_1_0, __string_1_1, ___x_0, ___x_1, __picture_2) {
    if (Sapl.feval(__typedecl_2_2)) {
        return __EdTab_tabDrawString$60_30(__keywordColour_3_7, __typedeclColour_3_9, __typedefColour_3_8, __commentColour_3_4, __charColour_3_6, __stringColour_3_5, __textColour_3_0, __tabColour_3_3, __EdTab_D(__clevel_2_0), Sapl.feval(__strings_4_0), __picture_2);
    } else {
        return __EdTab_tabDrawString$60_30(__keywordColour_3_7, __typedeclColour_3_9, __typedefColour_3_8, __commentColour_3_4, __charColour_3_6, __stringColour_3_5, __textColour_3_0, __tabColour_3_3, __EdTab_N(__clevel_2_0), Sapl.feval(__strings_4_0), __picture_2);
    }
};

function __EdTab_tabDrawStringC_select1$eval(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) {
    return __EdTab_tabDrawStringC_select1(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, Sapl.feval(a18), Sapl.feval(a19), Sapl.feval(a20));
};

function __EdTab_tabDrawStringC_select1(__strings_4_0, __textColour_3_0, __backgroundColour_3_1, __marginColour_3_2, __tabColour_3_3, __commentColour_3_4, __stringColour_3_5, __charColour_3_6, __keywordColour_3_7, __typedefColour_3_8, __typedeclColour_3_9, __clevel_2_0, __typedef_2_1, __typedecl_2_2, __offside_level_2_3, __flush_2_4, ___x_1_0, __string_1_1, ___x_0, ___x_1, __picture_2) {
    if (Sapl.feval(__typedef_2_1)) {
        return __EdTab_tabDrawString$60_30(__keywordColour_3_7, __typedeclColour_3_9, __typedefColour_3_8, __commentColour_3_4, __charColour_3_6, __stringColour_3_5, __textColour_3_0, __tabColour_3_3, __EdTab_T(__clevel_2_0), Sapl.feval(__strings_4_0), __picture_2);
    } else {
        return __EdTab_tabDrawStringC_select0(__strings_4_0, __textColour_3_0, __backgroundColour_3_1, __marginColour_3_2, __tabColour_3_3, __commentColour_3_4, __stringColour_3_5, __charColour_3_6, __keywordColour_3_7, __typedefColour_3_8, __typedeclColour_3_9, __clevel_2_0, __typedef_2_1, __typedecl_2_2, __offside_level_2_3, __flush_2_4, ___x_1_0, __string_1_1, ___x_0, ___x_1, __picture_2);
    }
};

function __EdTab_tabDrawStringC$eval(a0, a1, a2) {
    return __EdTab_tabDrawStringC(Sapl.feval(a0), Sapl.feval(a1), Sapl.feval(a2));
};

function __EdTab_tabDrawStringC(___x_0, ___x_1, __picture_2) {
    var ys = ___x_0;
    var ___x_1_0_1 = ys[2],
        __string_1_1_1 = ys[3];
    var ys = Sapl.feval(___x_1_0_1);
    var __clevel_2_0_2 = ys[2],
        __typedef_2_1_2 = ys[3],
        __typedecl_2_2_2 = ys[4],
        __offside_level_2_3_2 = ys[5],
        __flush_2_4_2 = ys[6];
    var ys = ___x_1;
    var __textColour_3_0_3 = ys[2],
        __backgroundColour_3_1_3 = ys[3],
        __marginColour_3_2_3 = ys[4],
        __tabColour_3_3_3 = ys[5],
        __commentColour_3_4_3 = ys[6],
        __stringColour_3_5_3 = ys[7],
        __charColour_3_6_3 = ys[8],
        __keywordColour_3_7_3 = ys[9],
        __typedefColour_3_8_3 = ys[10],
        __typedeclColour_3_9_3 = ys[11];
    var __strings_4_0_4 = __EdTab_splitAtTabs(Sapl.feval(__string_1_1_1));
    return __EdTab_tabDrawStringC_select1(__strings_4_0_4, __textColour_3_0_3, __backgroundColour_3_1_3, __marginColour_3_2_3, __tabColour_3_3_3, __commentColour_3_4_3, __stringColour_3_5_3, __charColour_3_6_3, __keywordColour_3_7_3, __typedefColour_3_8_3, __typedeclColour_3_9_3, __clevel_2_0_2, __typedef_2_1_2, __typedecl_2_2_2, __offside_level_2_3_2, __flush_2_4_2, ___x_1_0_1, __string_1_1_1, ___x_0, ___x_1, __picture_2);;
};

function __EdTab__SyntaxColours(__a1, __a2, __a3, __a4, __a5, __a6, __a7, __a8, __a9, __a10) {
    return [0, 'EdTab._SyntaxColours', __a1, __a2, __a3, __a4, __a5, __a6, __a7, __a8, __a9, __a10];
};
__EdTab__SyntaxColours.fields = ["EdTab.textColour", "EdTab.backgroundColour", "EdTab.marginColour", "EdTab.tabColour", "EdTab.commentColour", "EdTab.stringColour", "EdTab.charColour", "EdTab.keywordColour", "EdTab.typedefColour", "EdTab.typedeclColour"];

function __EdTab_DefaultSyntaxColours() {
    return __EdTab__SyntaxColours(__EdTab_Black(), __EdTab_White(), __EdTab_White(), __EdTab_Red(), __EdTab_Blue(), __EdTab_Green(), __EdTab_Magenta(), __EdTab_Magenta(), __EdTab_Red(), __EdTab_Red());
};

function __EdTab__Picture(__a1, __a2, __a3) {
    return [0, 'EdTab._Picture', __a1, __a2, __a3];
};
__EdTab__Picture.fields = ["EdTab.pencolor", "EdTab.len", "EdTab.out"];

function __EdTab_RGB(__a1) {
    return [0, 'EdTab.RGB', __a1];
};

function __EdTab_Black() {
    return [1, 'EdTab.Black'];
};

function __EdTab_White() {
    return [2, 'EdTab.White'];
};

function __EdTab_DarkGrey() {
    return [3, 'EdTab.DarkGrey'];
};

function __EdTab_Grey() {
    return [4, 'EdTab.Grey'];
};

function __EdTab_LightGrey() {
    return [5, 'EdTab.LightGrey'];
};

function __EdTab_Red() {
    return [6, 'EdTab.Red'];
};

function __EdTab_Green() {
    return [7, 'EdTab.Green'];
};

function __EdTab_Blue() {
    return [8, 'EdTab.Blue'];
};

function __EdTab_Cyan() {
    return [9, 'EdTab.Cyan'];
};

function __EdTab_Magenta() {
    return [10, 'EdTab.Magenta'];
};

function __EdTab_Yellow() {
    return [11, 'EdTab.Yellow'];
};

function __EdTab_newPicture() {
    return __EdTab__Picture(__EdTab_Black(), 0, ___predefined__Nil());
};

function ___predefined__Cons(__a1, __a2) {
    return [0, '_predefined._Cons', __a1, __a2];
};

function ___predefined__Nil() {
    return [1, '_predefined._Nil'];
};

function __Highlighter_genText(___x_0) {
    var ys = Sapl.feval(___x_0);
    switch (ys[0]) {
        case 0:
            var __l_1_0_1 = ys[2],
                __ls_1_1_1 = ys[3];
            return ___predefined__Cons([___predefined_tupsels2v0, [
                [__EdTab_asString, [
                    [__EdTab_tabDrawStringC$eval, [__l_1_0_1, __EdTab_DefaultSyntaxColours, __EdTab_newPicture]]
                ]]
            ]], [__Highlighter_genText, [__ls_1_1_1]]);
        case 1:
            return ___predefined__Nil();
    }
};

function __Highlighter_onClick_9(___x_0, ___x_1, ___x_2, __d_3) {
    var ___x_1_0_1 = [__SaplHtml_getDomAttr, [__d_3, "inp", "value"]];
    return ___predefined__Tuple2([___predefined_tupsels2v0, [
        [__SaplHtml_setDomAttr, [
            [___predefined_tupsels2v0, [___x_1_0_1]], "out", "innerHTML", [__Text_join_4$eval, ["<br/>", [__StdList_map$eval, [___predefined_tupsels2v0, [__StdList_flatten$eval, [
                [__Highlighter_genAnnotations, [
                    [__syncol_firstParse$eval, [
                        [__Highlighter_toStrictList, [
                            [__Text_split_11$eval, ["\n", [___predefined_tupsels2v1, [___x_1_0_1]]]]
                        ]]
                    ]]
                ]]
            ]]]]]]
        ]]
    ]], [___predefined_tupsels3v0, [___predefined__Tuple3(__Void_Void(), __syncol_quickParse$eval, __Highlighter_genText)]]);;
};

function __SystemTypes_NoValue() {
    return [0, 'SystemTypes.NoValue'];
};

function __SystemTypes_Value(__a1, __a2) {
    return [1, 'SystemTypes.Value', __a1, __a2];
};

function __SystemTypes_Unstable() {
    return [0, 'SystemTypes.Unstable'];
};

function __SystemTypes_Stable() {
    return [1, 'SystemTypes.Stable'];
};

function __Highlighter_anon_8(___x_0) {
    return __SystemTypes_Value(__Void_Void(), __SystemTypes_Unstable());
};

function __Void_Void() {
    return [0, 'Void.Void'];
};