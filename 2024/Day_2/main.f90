PROGRAM main

    USE levels_mod

    IMPLICIT NONE

    ! -----------------------------------------------------------------------------------------------

    INTEGER :: n_reports, n_safe, n_unsafe, n_safe_add
    ! n_reports :: Number of reports
    ! n_safe :: Number of safe reports
    ! n_unsafe :: Number of unsafe reports
    ! n_safe_add :: Number of safe reports to add, given by the problem dampener

    ! -----------------------------------------------------------------------------------------------

    INTEGER, ALLOCATABLE, DIMENSION(:) :: n_levels, reports_status
    ! n_levels :: Number of levels for each report (dimension will be n_reports)
    ! reports_status :: Labels reports as safe (1) or unsafe (0) (dimension will be n_reports)

    ! -----------------------------------------------------------------------------------------------

    TYPE(levels_arr), ALLOCATABLE, DIMENSION(:) :: reports, unsafe_reports
    ! TYPE(levels_arr) :: Integer 1-dim array called levels, allocatable with undefined dimension
    ! reports :: Will store the levels of each report. Level i_level in report i_report = reports(i_report)%levels(i_level)
    ! unsafe_reports :: Array elements will be the i_report integers of the unsafe reports (dimension will be number of unsafe reports)

    ! -----------------------------------------------------------------------------------------------

    CHARACTER(LEN=150) :: filepath, filename, format_key, tmp
    ! filepath :: Path to where the files are stored
    ! filename :: Name of a file to read or write to
    ! format_key :: Format character for READ or WRITE functions
    ! tmp :: Temporary character, if needed

    ! -----------------------------------------------------------------------------------------------

    INTEGER, PARAMETER :: debug_unit = 10
    ! -> debug_unit :: Debug file called reports_debug.dat

    ! -----------------------------------------------------------------------------------------------

    ! Input file name
    filepath = ""
    filename = "2024_day2_input_example.dat"

    ! Will read input file name, and output:
    ! -> n_reports is integer with number of reports
    ! -> n_levels is integer 1-dim array with dimension n_reports, with number of levels per report
    ! -> reports is a levels_arr 1-dim array with dimension n_reports, meaning reports(i_report) is integer 1-dim array called levels
    CALL read_reports(reports, n_reports, n_levels, filepath, filename)

    ! Opening a debug file
    filename = "reports_debug.dat"
    OPEN(UNIT = debug_unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "write", STATUS = "replace")

    ! Analyzing reports
    ALLOCATE(reports_status(n_reports))
    CALL analyzing_reports(debug_unit, n_reports, n_levels, reports, n_safe, reports_status)
    
    ! DEBUG
    WRITE(debug_unit,'(A,I5)')"Number of safe reports = ",n_safe
    WRITE(*,'(A,I5)')"Number of safe reports = ",n_safe

    ! Closing debug file
    CLOSE(UNIT = debug_unit)

    ! Opening a debug file for problem dampener
    filename = "problem_dampener.dat"
    OPEN(UNIT = debug_unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "write", STATUS = "replace")

    ! Building array whose entries are the unsafe reports; dimension of the array will be number of unsafe reports
    ! -> Deallocates reports_status, reports, unsafe_reports, n_levels
    n_unsafe = n_reports - n_safe
    CALL analyzing_unsafe_reports(debug_unit, n_reports, n_levels, reports, n_unsafe, reports_status, unsafe_reports, n_safe_add)

    ! Updating number of safe reports
    n_safe = n_safe + n_safe_add

    ! DEBUG
    WRITE(debug_unit,'(A,I5)')"Number of additional safe reports from problem dampener = ",n_safe_add
    WRITE(*,'(A,I5)')"Number of additional safe reports from problem dampener = ",n_safe_add
    WRITE(debug_unit,'(A,I5)')"Final number of safe reports = ",n_safe
    WRITE(*,'(A,I5)')"Final number of safe reports = ",n_safe

    ! Closing debug file
    CLOSE(UNIT = debug_unit)

    ! -----------------------------------------------------------------------------------------------

    CONTAINS

    SUBROUTINE read_reports(reports, n_reports, n_levels, filepath, filename)

        IMPLICIT NONE

        INTEGER, INTENT(INOUT) :: n_reports
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: n_levels
        TYPE(levels_arr), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports
        CHARACTER(LEN=*), INTENT(IN) :: filepath, filename

        ! -----------------------------------------------------------------------------------------------

        INTEGER :: io, j_row, j_char, real_len
        ! io :: Reading iostat for determining end of filename
        ! j_row :: A row of filename
        ! j_char :: A character of j_row (will be used in loop inside a DO of j_row)
        ! real_len :: Real length of a line of filename

        ! -----------------------------------------------------------------------------------------------

        CHARACTER(LEN=500) :: line
        ! line :: A line of filename. Length is an assumption of maximum length the file could have.

        ! -----------------------------------------------------------------------------------------------

        INTEGER, PARAMETER :: unit = 11
        ! unit :: For reading filename

        ! -----------------------------------------------------------------------------------------------

        ! Read the file a first time to get number of reports (rows)
        OPEN(UNIT = unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "read", STATUS = "old")

        io = 0 ! iostat that will determine end of file (0: Still within file; >0: Problem reading; <0: End of file)
        n_reports = 0 ! Number of rows

        DO WHILE (io == 0) ! While file has not reached its end
            n_reports = n_reports + 1 ! Add one to the number of rows
            READ(unit, *, IOSTAT = io) ! Read line just to get iostat
        END DO

        n_reports = n_reports - 1 ! An extra report is added due to the loop; this corrects it

        WRITE(*,'(A,I2)')"Checking io is negative (proving it reached end of file) :: io = ",io

        ! -----------------------------------------------------------------------------------------------

        ! Read the file a second time to get number of levels (columns)

        ! -> We assume each column is separated by one space, and one space only (a function can be implemented to search for gaps instead of single spaces)
        ! -> The number of columns in a row is given by the number of spaces + 1

        ALLOCATE(n_levels(n_reports)) ! Number of levels changes depending on the report

        REWIND(UNIT = unit) ! Go back to the start of the file

        DO j_row = 1, n_reports ! Go over the reports (rows)

            READ(unit, '(A)')line ! Get the full row
            real_len = LEN_TRIM(line) ! Get the real length of the row

            !WRITE(*,'(A,I8,A,I8)')">> Row number ",j_row," | Checking real length :: real_len = ",real_len

            n_levels(j_row) = 0 ! Initialize the number of levels at row j_row to 0

            DO j_char = 1, real_len ! Go through each individual character of row j_row

                !WRITE(*,'(A,I1,A)')"(*) ----> Character at index ",j_char," of line :: "//line(j_char:j_char)

                IF(line(j_char:j_char) == " ") THEN ! If current character is a single space
                    n_levels(j_row) = n_levels(j_row) + 1 ! Increase number of levels in row j_row
                END IF

            END DO

            n_levels(j_row) = n_levels(j_row) + 1 ! Number of levels (columns) = Number of spaces + 1

            !WRITE(*,'(A,I8,A)')">> Has ",n_levels(j_row)," integers, i.e, columns"
            !WRITE(*,'(A)')"--------------------------"

        END DO

        ! -----------------------------------------------------------------------------------------------

        ! Read file a final time to read the store the columns data
        ! -> Re-call, each report (row) has its own number of levels (columns)
        ! -> So we cannot store this data in a simple 2d matrix (number of columns would be equal to all rows)
        ! -> Therefore, in levels_mod, we create an allocatable integer 1-dim array as a new type, called levels_arr
        ! -> The variable name of levels_arr is levels
        ! -> We define reports as an allocatable levels_arr 1-dim array, meaning it is now a flexible 2-dim array
        ! -> To access:
        ! - - - > Row j_row, i.e, a report: reports(j_row)%levels(:)
        ! - - - > Level j_level of row j_row, i.e, a level: reports(j_row)%levels(j_level)

        ALLOCATE(reports(n_reports)) ! Allocates reports as a 1-dim array of dimension n_reports and of type levels_arr
        ! Still does not allocate what levels_arr requires

        REWIND(UNIT = unit) ! Go back to the begining of the file

        DO j_row = 1, n_reports ! Go over reports

            ALLOCATE(reports(j_row)%levels(n_levels(j_row))) ! Allocates the j_row entry of reports as a 1-dim array of dimension n_levels(j_row) and of type integer

            READ(unit, *)reports(j_row)%levels(:) ! Store the integers of row j_row into the array reports(j_row)%levels(:)

        END DO

        CLOSE(UNIT = unit) ! Close the file

    END SUBROUTINE read_reports

    ! -----------------------------------------------------------------------------------------------

    SUBROUTINE analyzing_reports(debug_unit, n_reports, n_levels, reports, n_safe, reports_status)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: n_reports, debug_unit
        INTEGER, INTENT(INOUT) :: n_safe
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(IN) :: n_levels
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports_status
        TYPE(levels_arr), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: reports

        ! -----------------------------------------------------------------------------------------------

        INTEGER :: i_report, n_levels_val
        ! i_report :: Array index of a report
        ! n_levels_val :: An entry of the integer array n_levels

        ! -----------------------------------------------------------------------------------------------

        ! Initialize number of safe reports to 0
        n_safe = 0

        ! DEBUG
        WRITE(debug_unit,'(A)')"STARTING reports analysis ..."
        WRITE(debug_unit,'(A)')""
        WRITE(debug_unit,'(A)')"-----------------------------------------------------------------------------------------------"
        WRITE(debug_unit,'(A)')""

        ! Going over reports
        DO i_report = 1, n_reports

            ! Number of levels in report i_report
            n_levels_val = n_levels(i_report)

            ! DEBUG
            WRITE(debug_unit,'(A,I5,A,I3)')"Report #",i_report," | Number of levels = ",n_levels_val

            ! -----------------------------------------------------------------------------------------------

            CALL safe_reports(debug_unit, i_report, reports(i_report)%levels(:), reports_status, n_safe, n_levels_val)

            ! DEBUG
            WRITE(debug_unit,'(A)')"-----------------------------------------------------------------------------------------------"
            WRITE(debug_unit,'(A)')""

        END DO ! End of reports loop

        ! DEBUG
        WRITE(debug_unit,'(A)')"ENDING reports analysis ..."

    END SUBROUTINE analyzing_reports

    ! -----------------------------------------------------------------------------------------------

    SUBROUTINE safe_reports(debug_unit, i_report, levels, reports_status, n_safe, n_levels_val)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: i_report, debug_unit, n_levels_val
        INTEGER, INTENT(INOUT) :: n_safe
        INTEGER, DIMENSION(n_levels_val), INTENT(IN) :: levels
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports_status

        ! -----------------------------------------------------------------------------------------------

        INTEGER :: i_level, current_level, prev_level, level_dif, prev_level_dif, dif_sign, prev_dif_sign
        ! i_level :: Array index of a level
        ! current_level :: Value of current level in a loop
        ! prev_level :: Value of the previous level in a loop
        ! level_dif :: Difference between current_level and prev_level
        ! prev_level_dif :: Previous level difference
        ! dif_sign :: Sign of difference between current_level and prev_level
        ! prev_dif_sign :: Sign of previous level difference

        ! -----------------------------------------------------------------------------------------------

        CHARACTER(LEN=10) :: dif_char, order_char
        ! dif_char :: Character of dif_flag
        ! order_char :: Character of order_flag

        ! -----------------------------------------------------------------------------------------------

        LOGICAL :: dif_flag, order_flag
        ! dif_flag :: For checking if difference in levels behaves according to requirement (inside [1,3])
        ! order_flag :: For checking if ordering of levels behaves according to requirement (all increasing or all decreasing)

        ! -----------------------------------------------------------------------------------------------

        ! Outside of level loop, looking at the first and second levels

        ! -> First level
        i_level = 1

        ! DEBUG
        WRITE(debug_unit,'(A,I3,A,I4)')"--> Level #",i_level," | Level value = ",levels(i_level)

        ! -> Second level
        i_level = 2

        ! Getting current level, previous level and the difference between the two
        current_level = levels(i_level)
        prev_level = levels(i_level-1)
        level_dif = current_level - prev_level

        ! DEBUG
        WRITE(debug_unit,'(A,I3,A,I4)')"--> Level #",i_level," | Level value = ",current_level
        WRITE(debug_unit,'(A,I2)')"--> [*] Difference with previous level = ",level_dif

        ! Evaluating level_dif :: Must be in the interval [1, 3]
        dif_flag = .TRUE. ! Only initialization of dif_flag; must not be reset within a report
        IF(abs(level_dif) < 1 .OR. abs(level_dif) > 3) THEN
            dif_flag = .FALSE.
        END IF

        ! Storing current level and difference as previous ones, before entering loop
        prev_level = current_level
        prev_level_dif = level_dif

        ! Go into the loop as the third level
        i_level = 3

        ! Enter loop with flag for ordering as TRUE, to trigger loop to start
        order_flag = .TRUE. ! Only initialization of order_flag; must not be reset within a report

        ! -----------------------------------------------------------------------------------------------

        ! Going over levels in report i_report until difference doesnt obey condition or we reach end of report
        DO WHILE(dif_flag .AND. order_flag .AND. i_level <= n_levels_val)

            ! Getting current level, previous level and the difference between the two
            current_level = levels(i_level)
            level_dif = current_level - prev_level

            ! DEBUG
            WRITE(debug_unit,'(A,I3,A,I4)')"--> Level #",i_level," | Level value = ",current_level
            WRITE(debug_unit,'(A,I2)')"--> [*] Difference with previous level = ",level_dif

            ! Evaluating level_dif :: Must be in the interval [1, 3]
            IF(abs(level_dif) < 1 .OR. abs(level_dif) > 3) THEN
                dif_flag = .FALSE.
            END IF

            ! Sign of current level difference (between i_level and i_level-1) and previous (between i_level-1 and i_level-2)
            dif_sign = SIGN(1, level_dif)
            prev_dif_sign = SIGN(1, prev_level_dif)

            ! Evaluating all increasing of all decreasing condition
            IF(dif_sign /= prev_dif_sign) THEN ! If difference changed sign, then it changed ordering
                order_flag = .FALSE.
            END IF

            ! Storing current level and difference as previous ones, for the next loop iteration
            prev_level = current_level
            prev_level_dif = level_dif

            ! Going to the next level
            i_level = i_level + 1

        END DO ! End of levels in report i_report loop

        ! -----------------------------------------------------------------------------------------------

        ! So, is the report safe?

        WRITE(dif_char,*)dif_flag
        WRITE(order_char,*)order_flag

        IF(dif_flag .AND. order_flag) THEN

            n_safe = n_safe + 1

            reports_status(i_report) = 1 ! Report i_report is labelled as safe

            ! DEBUG
            WRITE(debug_unit,'(A)')"Safe report :: dif_flag = "//TRIM(dif_char)//" | order_flag = "//TRIM(order_char)

        ELSE

            reports_status(i_report) = 0 ! Report i_report is labelled as unsafe

            ! DEBUG
            WRITE(debug_unit,'(A)')"Unsafe report :: dif_flag = "//TRIM(dif_char)//" | order_flag = "//TRIM(order_char)

        END IF

        ! DEBUG
        WRITE(debug_unit,'(A)')""

    END SUBROUTINE safe_reports

    ! -----------------------------------------------------------------------------------------------

    SUBROUTINE analyzing_unsafe_reports(debug_unit, n_reports, n_levels, reports, n_unsafe, reports_status, unsafe_reports, n_safe_add)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: n_reports, debug_unit, n_unsafe
        INTEGER, INTENT(INOUT) :: n_safe_add
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports_status, n_levels
        TYPE(levels_arr), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: reports, unsafe_reports

        ! -----------------------------------------------------------------------------------------------

        INTEGER :: i_report, n_levels_val, i_unsafe, i_level_remove, i_mask, prev_n_safe_add
        ! i_report :: Array index of a report
        ! n_levels_val :: An entry of the integer array n_levels
        ! i_unsafe :: Array index of only unsafe reports
        ! i_level_remove :: Array index of level to not consider in a loop
        ! i_mask :: Index for mask array inside PACK for unsafe_reports
        ! prev_n_safe_add :: Previous number of cumulative safe reports to add with problem dampener. To evaluate if there was a new safe report to add inside loop

        INTEGER, ALLOCATABLE, DIMENSION(:) :: n_levels_unsafe, mask_arr
        ! n_levels_unsafe :: n_levels analogue but just for unsafe reports
        ! mask_arr :: Array for masking PACK

        ! -----------------------------------------------------------------------------------------------

        ! Obtaining all unsafe reports and storing that in a new flexible 2-dim array
        ! -> Number of levels will also be reduced, to account only for the unsafe reports
        ALLOCATE(unsafe_reports(n_unsafe), n_levels_unsafe(n_unsafe))

        ! Unsafe reports array index
        i_unsafe = 1

        ! Go over all reports
        DO i_report = 1, n_reports

            ! Unsafe reports only
            IF(reports_status(i_report) == 0) THEN

                ! Number of levels of unsafe report; to store in n_levels_unsafe
                n_levels_val = n_levels(i_report)
                n_levels_unsafe(i_unsafe) = n_levels_val

                ! unsafe_reports is type(levels_arr), so each of its levels array needs to be allocated
                ALLOCATE(unsafe_reports(i_unsafe)%levels(n_levels_val))

                ! Storing the levels of the unsafe report
                unsafe_reports(i_unsafe)%levels(:) = reports(i_report)%levels(:)

                ! Advancing unsafe reports index
                i_unsafe = i_unsafe + 1

            END IF

        END DO

        ! We no longer need all of the reports nor the number of levels they each have
        ! The focus will be on unsafe_reports and n_levels_unsafe
        DEALLOCATE(reports, n_levels)

        ! -----------------------------------------------------------------------------------------------

        ! PROBLEM DAMPENER
        ! Now, for each unsafe report, let's remove one level, and see if it becomes safe
        ! -> We do this with PACK(unsafe_reports, (/ i_mask, i_mask = 1, n_levels_val /) /= i_level_remove )
        ! -> (/ i_mask, i_mask = 1, n_levels_val /) makes an array of all integers from 1 to n_levels_val
        ! -> /= i_level_remove transforms this into an array of booleans. All TRUE except the element with index i_level_remove, which will be FALSE
        ! -> PACK applies this mask to unsafe_reports
        ! The ones that do will give us a number of considered safe reports to add to the previous total

        ! Number of safe reports generated from the problem dampener
        n_safe_add = 0

        ! DEBUG
        WRITE(debug_unit,'(A)')"STARTING problem dampener analysis ..."
        WRITE(debug_unit,'(A)')""
        WRITE(debug_unit,'(A)')"-----------------------------------------------------------------------------------------------"
        WRITE(debug_unit,'(A)')""

        ! Go through the unsafe reports
        DO i_report = 1, n_unsafe

            ! Number of levels of current unsafe report i_report
            n_levels_val = n_levels_unsafe(i_report)

            ! Array that will remove one level at a time from current report, to study if it becomes safe or not
            ALLOCATE(mask_arr(n_levels_val))
            DO i_mask = 1, n_levels_val
                mask_arr(i_mask) = i_mask
            END DO

            ! DEBUG
            WRITE(debug_unit,'(A,I5,A,I3)')"Unsafe report #",i_report," | Number of levels = ",n_levels_val
            WRITE(debug_unit,'(A)')""

            ! Go through each level, but as a DO WHILE loop because we can stop after getting the first removed level that yields a safe report
            i_level_remove = 1
            prev_n_safe_add = n_safe_add
            DO WHILE(n_safe_add == prev_n_safe_add .AND. i_level_remove <= n_levels_val)

                WRITE(debug_unit,'(A)')"<><><><><><><><><><><><><><><>"
                WRITE(debug_unit,'(A,I2,A)')"<><>< Removed level # ",i_level_remove," ><><>"
                WRITE(debug_unit,'(A)')"<><><><><><><><><><><><><><><>"
                WRITE(debug_unit,'(A)')""

                ! We do the same procedure we did before the problem dampener
                ! But now, with an unsafe report that has had i_level_remove removed
                ! reports_status is here just to fill in the argument, we have no use for it anymore
                ! n_levels_val - 1 because thats the number of levels we have after removing one of them

                prev_n_safe_add = n_safe_add

                CALL safe_reports(debug_unit, i_report,&
                PACK(unsafe_reports(i_report)%levels(:), mask_arr /= i_level_remove ),&
                reports_status, n_safe_add, n_levels_val-1)

                i_level_remove = i_level_remove + 1

            END DO

            ! Mask array no longer needed
            DEALLOCATE(mask_arr)

            ! DEBUG
            WRITE(debug_unit,'(A,I4)')"Number of safe reports to add so far (cumulative) = ",n_safe_add
            WRITE(debug_unit,'(A)')""
            WRITE(debug_unit,'(A)')"-----------------------------------------------------------------------------------------------"
            WRITE(debug_unit,'(A)')""

        END DO

        ! No longer need to keep any of the reports arrays
        DEALLOCATE(unsafe_reports, reports_status, n_levels_unsafe)

        ! DEBUG
        WRITE(debug_unit,'(A)')"ENDING problem dampener analysis ..."

    END SUBROUTINE analyzing_unsafe_reports

END PROGRAM main
