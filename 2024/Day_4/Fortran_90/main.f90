PROGRAM main

    IMPLICIT NONE

    CHARACTER(LEN=150) :: filepath, filename
    ! filepath :: Path to where the files are stored
    ! filename :: Name of a file to read or write to

    ! -----------------------------------------------------------------------------------------------

    INTEGER, PARAMETER :: debug_unit = 10
    ! -> debug_unit :: Debug file called reports_debug.dat

    ! -----------------------------------------------------------------------------------------------

    ! Input file name
    filepath = ""

    ! Opening a debug file
    filename = "word_search_debug.txt"
    OPEN(UNIT = debug_unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "write", STATUS = "replace")

    filename = "2024_day4_input_example.txt"
    CALL read_file(debug_unit, filepath, filename)

    ! Closing debug file
    CLOSE(UNIT = debug_unit)

    ! -----------------------------------------------------------------------------------------------

    CONTAINS

    SUBROUTINE read_file(debug_unit, filepath, filename)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: debug_unit
        CHARACTER(LEN=*), INTENT(IN) :: filepath, filename

        ! -----------------------------------------------------------------------------------------------

        INTEGER :: io, n_rows, n_columns, i_row, i_column, x_count, i_x, n_states, i_state, state,&
        i_row_direction, i_column_direction, xmas_count
        INTEGER, ALLOCATABLE, DIMENSION(:) :: boundary_status, states_to_check
        INTEGER, ALLOCATABLE, DIMENSION(:, :) :: x_pairs
        ! io :: For READ to determine end of file
        ! n_rows :: Number of rows of file
        ! n_columns :: Number of columns of file
        ! i_row :: Index of file row
        ! i_column :: Index of file column
        ! x_count :: Number of X characters is file_mat
        ! x_pairs :: Index pairs of X characters in file_mat
        ! i_x :: Ordinal number of the X character, from top to bottom, left to right
        ! boundary_status :: Status of row and column in terms of boundary
        ! states_to_check :: States to check on the XMAS FSM
        ! n_states :: Number of states to check
        ! i_state :: Index of states_to_check
        ! state :: states_to_check(i_state)
        ! i_row_direction, i_column_direction :: Row and column directions to check for
        ! xmas_count :: Count of the word XMAS in the word search

        ! -----------------------------------------------------------------------------------------------

        CHARACTER(LEN=10) :: format_key
        CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:, :) :: file_mat
        CHARACTER(LEN=1000) :: line
        ! file_char :: A character of the file
        ! file_mat :: Contents of the file in a 2D matrix
        ! line :: Using line to get number of columns of file. Length is assumed
        ! format_key :: For custom format on a READ or WRITE command

        ! -----------------------------------------------------------------------------------------------

        INTEGER, PARAMETER :: unit = 11
        ! unit :: For reading filename

        ! -----------------------------------------------------------------------------------------------

        ! Read the file a first time to get number of rows
        ! Also use the first line to get number of columns
        ! Here, there are no spaces separating letters and all rows have the same number of columns
        ! This really is a matrix of letters
        OPEN(UNIT = unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "read", STATUS = "old")

        io = 0
        n_rows = 0

        ! First line is done seperatly to count number of columns
        n_rows = n_rows + 1
        READ(unit,'(A)',IOSTAT=io)line
        n_columns = LEN_TRIM(line)

        ! Proceed to the rest of the file
        DO WHILE(io == 0)
            n_rows = n_rows + 1
            READ(unit,'(A)', IOSTAT=io)
        END DO
        n_rows = n_rows - 1

        ! DEBUG
        WRITE(debug_unit,'(A,I4,A,I4,A)')"File has ",n_rows," rows and ",n_columns," columns. Store it into a 2-dim array"
        WRITE(debug_unit,'(A)')""

        ! -----------------------------------------------------------------------------------------------

        ! Rewind the store matrix of letters into a 2-dim array
        REWIND(UNIT = unit) ! Go back to the start of the file

        ALLOCATE(file_mat(n_rows, n_columns))

        WRITE(format_key,'(A,I4,A)')"(",n_rows,"A1)"

        ! DEBUG
        WRITE(debug_unit,'(A)')"Reproducing file content:"

        DO i_row = 1, n_rows

            READ(unit,'(A)')line

            DO i_column = 1, n_columns
                file_mat(i_row, i_column) = line(i_column:i_column)
            END DO

            ! DEBUG
            WRITE(debug_unit, TRIM(format_key))file_mat(i_row,:)

        END DO

        ! DEBUG
        WRITE(debug_unit,'(A)')""

        ! -----------------------------------------------------------------------------------------------

        ! Step 1 :: Find all (i_row, i_column) pairs of the letter "X"

        x_count = COUNT(file_mat == "X") ! Does not give the index pairs but how many X characters the matrix has

        ALLOCATE(x_pairs(2, x_count)) ! Index 1 is row, Index 2 is column

        i_x = 1

        IF(x_count > 0) THEN ! No point in searching pairs if there are no X characters to begin with. In principle, of course, there are

            DO i_column = 1, n_columns ! Column-major order loop, more appropriate for Fortran
                DO i_row = 1, n_rows

                    IF(file_mat(i_row, i_column) == "X") THEN

                        x_pairs(1, i_x) = i_row
                        x_pairs(2, i_x) = i_column

                        i_x = i_x + 1 ! Advancing to the next X

                    END IF

                END DO
            END DO

        END IF

        ! DEBUG
        WRITE(debug_unit,'(A,I6,A)')"Found ",x_count," X characters in word search matrix"
        WRITE(debug_unit,'(A)')""

        ! -----------------------------------------------------------------------------------------------

        ! Step 2 :: Go to each X location and do the XMAS search FSM

        xmas_count = 0

        DO i_x = 1, x_count ! Go though all X locations

            i_row = x_pairs(1, i_x) ! Row of current X
            i_column = x_pairs(2, i_x) ! Column of current X

            ! DEBUG
            WRITE(debug_unit,'(A,I6,A,I4,A,I4,A)')"At X location #",i_x," | (Row, column) = (",i_row,", ",i_column,")"
            WRITE(debug_unit,'(A)')""

            ! XMAS search FSM

            ! Q1 : Is row near a boundary?

            ALLOCATE(boundary_status(2)) ! Index 1 is for row status, Index 2 is for column status
            ! In terms of values: -1 if row is left / column is bottom, 0 if no boundary, 1 if row is right / column is top

            IF(i_row == 1) THEN ! Top boundary
                boundary_status(1) = 1
            ELSE IF(i_row == n_rows) THEN ! Bottom boundary
                boundary_status(1) = -1
            ELSE ! No boundary
                boundary_status(1) = 0
            END IF

            ! Q2 : Is column near a boundary?

            IF(i_column == 1) THEN ! Left boundary
                boundary_status(2) = -1
            ELSE IF(i_column == n_columns) THEN ! Right boundary
                boundary_status(2) = 1
            ELSE ! No boundary
                boundary_status(2) = 0
            END IF

            ! DEBUG
            WRITE(debug_unit,'(A,I2,A,I2)')"--> Boundary status | Row :: ",boundary_status(1)," ; Column :: ",boundary_status(2)

            ! Boundary possibilities:
            ! boundary_status(1) == 1 .AND. boundary_status(2) == -1 :: Top-left corner
            ! boundary_status(1) == 1 .AND. boundary_status(2) == 0 :: Top wall
            ! boundary_status(1) == 1 .AND. boundary_status(2) == 1 :: Top-right corner
            ! boundary_status(1) == 0 .AND. boundary_status(2) == 1 :: Right wall
            ! boundary_status(1) == -1 .AND. boundary_status(2) == 1 :: Bottom-right corner
            ! boundary_status(1) == -1 .AND. boundary_status(2) == 0 :: Bottom wall
            ! boundary_status(1) == -1 .AND. boundary_status(2) == -1 :: Bottom-left corner
            ! boundary_status(1) == 0 .AND. boundary_status(2) == -1 :: Left wall
            ! boundary_status(1) == 0 .AND. boundary_status(2) == 0 :: No boundaries

            ! Q3 : Boundary check
            IF(boundary_status(1) == 1 .AND. boundary_status(2) == -1) THEN ! Top-left corner

                n_states = 3
                ALLOCATE(states_to_check(n_states)) ! Corners have 3 states to check for

                states_to_check = (/ 4, 5, 6 /)

            ELSE IF(boundary_status(1) == 1 .AND. boundary_status(2) == 0) THEN ! Top wall

                n_states = 5
                ALLOCATE(states_to_check(n_states)) ! Walls have 5 states to check for

                states_to_check = (/ 4, 5, 6, 7, 8 /)

            ELSE IF(boundary_status(1) == 1 .AND. boundary_status(2) == 1) THEN ! Top-right corner

                n_states = 3
                ALLOCATE(states_to_check(n_states)) ! Corners have 3 states to check for

                states_to_check = (/ 6, 7, 8 /)

            ELSE IF(boundary_status(1) == 0 .AND. boundary_status(2) == 1) THEN ! Right wall

                n_states = 5
                ALLOCATE(states_to_check(n_states)) ! Walls have 5 states to check for

                states_to_check = (/ 6, 7, 8, 1, 2 /)

            ELSE IF(boundary_status(1) == -1 .AND. boundary_status(2) == 1) THEN ! Bottom-right corner

                n_states = 3
                ALLOCATE(states_to_check(n_states)) ! Corners have 3 states to check for

                states_to_check = (/ 8, 1, 2 /)

            ELSE IF(boundary_status(1) == -1 .AND. boundary_status(2) == 0) THEN ! Bottom wall

                n_states = 5
                ALLOCATE(states_to_check(n_states)) ! Walls have 5 states to check for

                states_to_check = (/ 8, 1, 2, 3, 4 /)

            ELSE IF(boundary_status(1) == -1 .AND. boundary_status(2) == -1) THEN ! Bottom-left corner

                n_states = 3
                ALLOCATE(states_to_check(n_states)) ! Corners have 3 states to check for

                states_to_check = (/ 2, 3, 4 /)

            ELSE IF(boundary_status(1) == 0 .AND. boundary_status(2) == -1) THEN ! Left wall

                n_states = 5
                ALLOCATE(states_to_check(n_states)) ! Walls have 5 states to check for

                states_to_check = (/ 2, 3, 4, 5, 6 /)

            ELSE IF(boundary_status(1) == 0 .AND. boundary_status(2) == 0) THEN ! No boundaries

                n_states = 8
                ALLOCATE(states_to_check(n_states)) ! No boundaries means we have to check all 8 states

                states_to_check = (/ 1, 2, 3, 4, 5, 6, 7, 8 /)

            END IF ! End of boundary check

            ! Q4 : States
            ! State 1 :: M at top-left of X
            ! State 2 :: M at top of X
            ! State 3 :: M at top-right of X
            ! State 4 :: M at right of X
            ! State 5 :: M at bottom-right of X
            ! State 6 :: M at bottom of X
            ! State 7 :: M at bottom-left of X
            ! State 8 :: M at left of X

            ! States to check
            DO i_state = 1, n_states

                state = states_to_check(i_state) ! Current state to check

                ! DEBUG
                WRITE(debug_unit,'(A,I1,A,I1,A,I1,A)')"--> Checking state ",state," (",i_state," out of ",n_states," possible states)"

                ! Top-left is (1, 1)
                ! Top-right is (1, n_columns)
                ! Bottom-left is (n_rows, 1)
                ! Bottom-right is (n_rows, n_columns)

                SELECT CASE(state)
                    CASE(1) ! M at top-left of X

                        i_row_direction = -1 ! Upwards
                        i_column_direction = -1 ! To the left

                    CASE(2) ! M at top of X

                        i_row_direction = -1 ! Upwards
                        i_column_direction = 0 ! Stays in the same column

                    CASE(3) ! M at top-right of X

                        i_row_direction = -1 ! Upwards
                        i_column_direction = 1 ! To the right

                    CASE(4) ! M at right of X

                        i_row_direction = 0 ! Stay in the same row
                        i_column_direction = 1 ! To the right

                    CASE(5) ! M at bottom-right of X

                        i_row_direction = 1 ! Downwards
                        i_column_direction = 1 ! To the right

                    CASE(6) ! M at bottom of X

                        i_row_direction = 1 ! Downwards
                        i_column_direction = 0 ! Stays in the same column

                    CASE(7) ! M at bottom-left of X

                        i_row_direction = 1 ! Downwards
                        i_column_direction = -1 ! To the left

                    CASE(8) ! M at left of X

                        i_row_direction = 0 ! Stay in the same row
                        i_column_direction = -1 ! To the left

                END SELECT

                ! DEBUG
                WRITE(debug_unit,'(A,I2,A,I2)')"--> Direction to check | Row dir = ",i_row_direction," ; Column dir = ",i_column_direction

                ! Checking next letter
                CALL check_next_letter(debug_unit, i_row_direction, i_row, n_rows, i_column_direction, i_column, n_columns, file_mat, xmas_count)

                ! DEBUG
                WRITE(debug_unit,'(A)')""

            END DO ! End of going over states to check

            ! After no longer needing to check states
            DEALLOCATE(states_to_check)

            ! After no longer needing to check boundary
            DEALLOCATE(boundary_status)

            ! DEBUG
            WRITE(debug_unit,'(A)')""
            WRITE(debug_unit,'(A)')"---------------------------------------------------------------------------------------"
            WRITE(debug_unit,'(A)')""

        END DO ! End of going through X locations

        ! -----------------------------------------------------------------------------------------------

        DEALLOCATE(x_pairs, file_mat)
        CLOSE(UNIT = unit)

        ! DEBUG
        WRITE(debug_unit,'(A,I6)')"--> Final count of XMAS words | xmas_count = ",xmas_count
        WRITE(*,'(A,I6)')"--> Final count of XMAS words | xmas_count = ",xmas_count

    END SUBROUTINE read_file

    ! -----------------------------------------------------------------------------------------------

    SUBROUTINE check_next_letter(debug_unit, i_row_direction, i_row, n_rows, i_column_direction, i_column, n_columns, file_mat, xmas_count)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: i_row_direction, i_column_direction, i_row, i_column, n_rows, n_columns, debug_unit
        INTEGER, INTENT(INOUT) :: xmas_count
        CHARACTER(LEN=1), ALLOCATABLE, DIMENSION(:, :) :: file_mat

        INTEGER :: i_row_check, i_column_check
        LOGICAL :: row_flag, column_flag

        ! -----------------------------------------------------------------------------------------------

        i_row_check = i_row + i_row_direction
        i_column_check = i_column + i_column_direction

        ! DEBUG
        WRITE(debug_unit,'(A,I4,A,I4,A,I4,A,I4,A)')"--> Direction visualized as (Row, column) | (",i_row," ,",i_column,") >> (",i_row_check,", ",i_column_check,")"

        IF(file_mat(i_row_check, i_column_check) == "M") THEN

            ! DEBUG
            WRITE(debug_unit,'(A)')"--> Character IS M (checking :: "//file_mat(i_row_check, i_column_check)//") and away from/at the boundary"

            ! Keeping checking in the same direction, now for A. Make sure it is not a boundary
            i_row_check = i_row_check + i_row_direction
            i_column_check = i_column_check + i_column_direction

            row_flag = i_row_check >= 1 .AND. i_row_check <= n_rows
            column_flag = i_column_check >= 1 .AND. i_column_check <= n_columns

            ! DEBUG
            WRITE(debug_unit,'(A,I4,A,I4,A)')"--> Go in the same direction >> (",i_row_check,", ",i_column_check,")"

            IF(file_mat(i_row_check, i_column_check) == "A" .AND. row_flag .AND. column_flag) THEN

                ! DEBUG
                WRITE(debug_unit,'(A)')"--> Character IS A (checking :: "//file_mat(i_row_check, i_column_check)//") and away from/at the boundary"

                ! Keeping checking in the same direction, now for S. Make sure it is not a boundary
                i_row_check = i_row_check + i_row_direction
                i_column_check = i_column_check + i_column_direction

                row_flag = i_row_check >= 1 .AND. i_row_check <= n_rows
                column_flag = i_column_check >= 1 .AND. i_column_check <= n_columns

                ! DEBUG
                WRITE(debug_unit,'(A,I4,A,I4,A)')"--> Go in the same direction >> (",i_row_check,", ",i_column_check,")"

                IF(file_mat(i_row_check, i_column_check) == "S" .AND. row_flag .AND. column_flag) THEN

                    ! DEBUG
                    WRITE(debug_unit,'(A)')"--> Character IS S (checking :: "//file_mat(i_row_check, i_column_check)//")"

                    xmas_count = xmas_count + 1

                    ! DEBUG
                    WRITE(debug_unit,'(A,I6)')"--> XMAS word has been formed. Updating count | xmas_count = ",xmas_count

                ELSE

                    IF(row_flag .AND. column_flag) THEN

                        ! DEBUG
                        WRITE(debug_unit,'(A)')"--> Character is NOT S (checking :: "//file_mat(i_row_check, i_column_check)//") | Moving on"

                    ELSE

                        ! DEBUG
                        WRITE(debug_unit,'(A)')"--> Character goes beyond a boundary, cannot form XMAS word | Moving on"

                    END IF

                END IF

            ELSE

                IF(row_flag .AND. column_flag) THEN

                    ! DEBUG
                    WRITE(debug_unit,'(A)')"--> Character is NOT A (checking :: "//file_mat(i_row_check, i_column_check)//") | Moving on"

                ELSE

                    ! DEBUG
                    WRITE(debug_unit,'(A)')"--> Character goes beyond a boundary, cannot form XMAS word | Moving on"

                END IF

            END IF
        
        ELSE

            ! DEBUG
            WRITE(debug_unit,'(A)')"--> Character is NOT M (checking :: "//file_mat(i_row_check, i_column_check)//") | Moving on"

        END IF

    END SUBROUTINE check_next_letter

END PROGRAM main
