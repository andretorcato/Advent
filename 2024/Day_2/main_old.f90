PROGRAM main

    USE one_dim_arr_mod

    IMPLICIT NONE

    INTEGER :: n_reports, i_report, i_level, n_levels_val, n_safe,&
    current_level, prev_level, ascend_score, descend_score, same_score, level_dif
    INTEGER, ALLOCATABLE, DIMENSION(:) :: n_levels, problematic_levels
    TYPE(one_dim_array), ALLOCATABLE, DIMENSION(:) :: reports
    CHARACTER(LEN=150) :: filepath, filename, format_key, tmp
    CHARACTER(LEN=10) :: dif_char, ascend_char, descend_char
    LOGICAL :: dif_flag, ascend_flag, descend_flag, local_dif_flag

    INTEGER, PARAMETER :: debug_unit = 10

    filepath = ""
    filename = "2024_day2_input.dat"

    ! Reading reports

    CALL read_reports(reports, n_reports, n_levels, filepath, filename)

    filename = "reports_debug.dat"

    OPEN(UNIT = debug_unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "write", STATUS = "replace")

    ! Determining number of safe reports
    ! -> Levels must be always increasing or always decreasing
    ! -> Two adjacent levels must differ by: at least 1 or at most 3

    n_safe = 0

    WRITE(debug_unit,'(A)')"Checking reports:"
    WRITE(debug_unit,'(A)')""

    DO i_report = 1, n_reports

        n_levels_val = n_levels(i_report)

        ascend_score = 0
        descend_score = 0
        same_score = 0

        dif_flag = .TRUE. ! Default is that it obeys condition

        WRITE(debug_unit,'(A,I3,A,I3)')">> Report number ",i_report," | Number of levels = ",n_levels_val
        WRITE(debug_unit,'(A,I3)')"(*) ----> Level number 1 | ",reports(i_report)%levels(1)

        ALLOCATE(problematic_levels(n_levels_val-1))

        DO i_level = 2, n_levels_val ! Beings one level ahead, compares with previous

            !reports(i_report)%levels(i_level)

            current_level = reports(i_report)%levels(i_level)
            prev_level = reports(i_report)%levels(i_level-1)
            level_dif = current_level-prev_level

            !WRITE(tmp,'(I3)')n_levels_val
            !format_key = '(A,I3,A,'//TRIM(tmp)//'I4)'
            WRITE(debug_unit,'(A,I3,A,I3)')"(*) ----> Level number ",i_level," | ",current_level

            ! Analyzing if it is always increasing or decreasing. Will only know at the end of i_level loop
            ! To be always increasing: ascend_score == n_levels_val-1 and descend_score == same_score == 0
            ! To be always decreasing: descend_score == n_levels_val-1 and ascend_score == same_score == 0
            ! Any other combination of values will result in an unsafe report

            IF(level_dif > 0) THEN
                ascend_score = ascend_score + 1
            ELSE IF(level_dif < 0) THEN
                descend_score = descend_score + 1
            ELSE
                same_score = same_score + 1
            END IF

            ! Analyzing if the differences between adjacent levels are always inside [1, 3]
            ! Just has to happen once for dif_flag to be FALSE for the rest of the report

            IF(abs(level_dif) < 1 .OR. abs(level_dif) > 3) THEN
                dif_flag = .FALSE.
                local_dif_flag = .FALSE.
            ELSE
                local_dif_flag = .TRUE.
            END IF

            ! For storing if level is problematic

            ascend_flag = ascend_score > 0 .AND. descend_score == 0 .AND. same_score == 0
            descend_flag = descend_score > 0 .AND. ascend_score == 0 .AND. same_score == 0

            IF(local_dif_flag .AND. ( ascend_flag .OR. descend_flag )) THEN
                problematic_levels(i_level) = 0 ! Not a problem
            ELSE
                problematic_levels(i_level) = 1 ! Problem
            END IF

            WRITE(debug_unit,'(A,I3,A,I3,A,I3,A,I3,A)')"(*) [Dif with prev ",level_dif," | Ascend score = ",ascend_score,&
            " | Descend score = ",descend_score," | Same score = ",same_score,"]"

        END DO

        ! Report analyzes summaries

        ascend_flag = ascend_score == n_levels_val-1 .AND. descend_score == 0 .AND. same_score == 0
        descend_flag = descend_score == n_levels_val-1 .AND. ascend_score == 0 .AND. same_score == 0

        WRITE(dif_char,*)dif_flag
        WRITE(ascend_char,*)ascend_flag
        WRITE(descend_char,*)descend_flag

        IF(dif_flag .AND. ( ascend_flag .OR. descend_flag )) THEN
            n_safe = n_safe + 1
            WRITE(debug_unit,'(A)')"Safe report | dif_flag = "//TRIM(dif_char)//" | ascend_flag = "//TRIM(ascend_char)//" | descend_flag = "//TRIM(descend_char)
        ELSE

            ! Problem dampener
            ! -> If by removing one level the report becomes safe, consider the report as safe

            ! Ideas
            ! -> Store levels that caused problems
            ! -> Then check what happens to the report if those levels are removed, one by one
            ! -> Consider report safe the first removed level that yields a safe report (no need to check the other possibilities)



            WRITE(debug_unit,'(A)')"Unsafe report | dif_flag = "//TRIM(dif_char)//" | ascend_flag = "//TRIM(ascend_char)//" | descend_flag = "//TRIM(descend_char)
        END IF

        WRITE(debug_unit,'(A)')""

        DEALLOCATE(problematic_levels)

    END DO

    WRITE(*,'(A,I4)')"Number of safe reports = ",n_safe

    CLOSE(UNIT = debug_unit)

    DEALLOCATE(n_levels, reports)

    ! -----------------------------------------------------------------------------------------------

    CONTAINS

    SUBROUTINE read_reports(reports, n_rows, n_cols, filepath, filename)

        INTEGER, INTENT(INOUT) :: n_rows
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: n_cols
        TYPE(one_dim_array), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports
        CHARACTER(LEN=*), INTENT(IN) :: filepath, filename

        INTEGER :: io, j_row, j_char, real_len
        CHARACTER(LEN=500) :: line ! Needs to be larger enough of an assumption so that a real line in the file is not larger then this LEN
        INTEGER, PARAMETER :: unit = 11

        ! Read the file a first time to get number of rows
        
        OPEN(UNIT = unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "read", STATUS = "old")

        io = 0
        n_rows = 0

        DO WHILE (io == 0)
            n_rows = n_rows + 1
            READ(unit, *, IOSTAT = io)
        END DO

        n_rows = n_rows - 1 ! Subtract extra row that is added by the DO WHILE loop

        WRITE(*,'(A,I8)')"Checking io is negative (proving it reached end of file) :: io = ",io

        ! Read the file a second time to get number of columns

        ALLOCATE(n_cols(n_rows))

        REWIND(UNIT = unit)

        DO j_row = 1, n_rows

            READ(unit, '(A)')line ! Get the full row

            real_len = LEN_TRIM(line) ! Get the real length of the row

            !WRITE(*,'(A,I8,A,I8)')">> Row number ",j_row," | Checking real length :: real_len = ",real_len

            n_cols(j_row) = 0

            DO j_char = 1, real_len
                !WRITE(*,'(A,I1,A)')"(*) ----> Character at index ",j_char," of line :: "//line(j_char:j_char)
                IF(line(j_char:j_char) == " ") THEN ! Im going to assume the integers are all separated by ONE space only; otherwise, I have to implement a function for GAPS, instead of single spaces
                    n_cols(j_row) = n_cols(j_row) + 1
                END IF
            END DO

            ! Number of columns, i.e, number of different integers in a line is equal to number of spaces + 1
            ! The code was done assuming a separation of ONE SPACE between two integers
            ! A function would have to be implemented to count the number of GAPS if the separation between two integers has variable number of spaces

            n_cols(j_row) = n_cols(j_row) + 1

            !WRITE(*,'(A,I8,A)')">> Has ",n_cols(j_row)," integers, i.e, columns"
            !WRITE(*,'(A)')"--------------------------"

        END DO

        ! Read file a final time to read the integers and store them into the 2-dim array

        ALLOCATE(reports(n_rows))

        REWIND(UNIT = unit)

        DO j_row = 1, n_rows

            ALLOCATE(reports(j_row)%levels(n_cols(j_row)))

            READ(unit, *)reports(j_row)%levels(:)

        END DO

        !ALLOCATE(reports(n_rows, n_cols))
        !REWIND(UNIT = unit)
        !DO j1 = 1, n_rows
        !    READ(unit, *)left_list(j1), right_list(j1)
        !END DO

        CLOSE(UNIT = unit)

    END SUBROUTINE read_reports

    ! -----------------------------------------------------------------------------------------------

END PROGRAM main
