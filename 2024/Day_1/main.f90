PROGRAM main

    IMPLICIT NONE

    INTEGER :: n_rows, i1, user_choice, min_left_pos_list(1), min_left_pos, min_right_pos_list(1), min_right_pos, pair_dif, total_sum
    INTEGER, ALLOCATABLE, DIMENSION(:) :: left_list, right_list, dum_left_list, dum_right_list
    CHARACTER(LEN=150) :: filepath, filename

    filepath = ""
    filename = "2024_day1_input_example.dat"

    ! Reading left and right lists

    CALL read_two_columns(left_list, right_list, n_rows, filepath, filename)

    WRITE(*,'(A)')"Checking lists:"
    DO i1 = 1, n_rows
        WRITE(*,'(A,I6,A,I6,A,I6)')">> i1 = ",i1," | Left list element = ",left_list(i1)," | Right list element = ",right_list(i1)
    END DO

    ! Asking user which algorithm to use to solve the puzzle

    WRITE(*,'(A)')"Which method do you want to use to get the answer to the puzzle?"
    WRITE(*,'(A)')"(Option 1 :: Order the arrays and pair the elements in increasing order of the index (WIP)"
    WRITE(*,'(A)')"(Option 2 :: Get the minimum from each list, pair them up, then remove the minimum, repeat until lists are empty)"
    WRITE(*,'(A)')"Answer: "
    READ(*,*)user_choice

    IF (user_choice == 1) THEN ! Ordering

        WRITE(*,'(A)')"You chose :: Ordering (WIP)"

    ELSE IF(user_choice == 2) THEN ! Min and remove

        WRITE(*,'(A)')"You chose :: Minimum and removal"

        WRITE(*,'(A)')"----------"

        total_sum = 0

        DO WHILE (n_rows > 0)

            WRITE(*,'(A,I8)')">> Current number of rows = ",n_rows

            min_left_pos_list = MINLOC(left_list)
            min_left_pos = min_left_pos_list(1)

            min_right_pos_list = MINLOC(right_list)
            min_right_pos = min_right_pos_list(1)

            WRITE(*,'(A,I8,A,I8)')">> Minimum index of left list = ",min_left_pos," | Minimum value = ",left_list(min_left_pos)
            WRITE(*,'(A,I8,A,I8)')">> Minimum index of right list = ",min_right_pos," | Minimum value = ",right_list(min_right_pos)

            pair_dif = abs(left_list(min_left_pos)-right_list(min_right_pos))
            total_sum = total_sum + pair_dif

            WRITE(*,'(A,I8,A,I8)')">> Pair difference = ",pair_dif," | Total sum = ",total_sum

            ALLOCATE(dum_left_list(n_rows), dum_right_list(n_rows))

            dum_left_list = left_list
            dum_right_list = right_list

            DEALLOCATE(left_list, right_list)

            n_rows = n_rows - 1 ! Lets remove the minimum value

            ALLOCATE(left_list(n_rows), right_list(n_rows))

            left_list(1:min_left_pos-1) = dum_left_list(1:min_left_pos-1)
            left_list(min_left_pos:n_rows) = dum_left_list(min_left_pos+1:n_rows+1)

            right_list(1:min_right_pos-1) = dum_right_list(1:min_right_pos-1)
            right_list(min_right_pos:n_rows) = dum_right_list(min_right_pos+1:n_rows+1)

            DEALLOCATE(dum_left_list, dum_right_list)

            WRITE(*,'(A)')"----------"

        END DO

    END IF

    ! -----------------------------------------------------------------------------------------------

    CONTAINS

    SUBROUTINE read_two_columns(left_list, right_list, n_rows, filepath, filename)

        INTEGER, INTENT(INOUT) :: n_rows
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: left_list, right_list
        CHARACTER(LEN=*), INTENT(IN) :: filepath, filename

        INTEGER :: io, j1
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

        ! Read file second time to read the integers and store them into the lists

        ALLOCATE(left_list(n_rows), right_list(n_rows))

        REWIND(UNIT = unit)

        DO j1 = 1, n_rows
            READ(unit, *)left_list(j1), right_list(j1)
        END DO

        CLOSE(UNIT = unit)

    END SUBROUTINE read_two_columns

    ! -----------------------------------------------------------------------------------------------

END PROGRAM main
