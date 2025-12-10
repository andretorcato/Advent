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
    filename = "mul_debug.txt"
    OPEN(UNIT = debug_unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "write", STATUS = "replace")

    filename = "2024_day3_input.txt"
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

        INTEGER :: io, n_rows, row_count, state, is_int, i_x, i_y, X, Y, read_int, X_wip(3), Y_wip(3), n_clean, summ, i_dum,&
        enable_instructions, instruct_state
        INTEGER, ALLOCATABLE, DIMENSION(:) :: X_clean, Y_clean
        ! io :: For READ to determine end of file
        ! n_rows :: Number of rows of file
        ! row_count :: To count number of rows
        ! state :: State of the character, for the FSM
        ! is_int :: 0 if char is an integer, 1 if char is not
        ! i_x :: Index for character X
        ! i_y :: Index for character Y
        ! X :: Integer X
        ! Y :: Integer Y
        ! read_int :: Read integer from is_it_int subroutine
        ! X_wip :: Array of each digit of X
        ! Y_wip :: Array of each digit of Y
        ! n_clean :: Size of X_clean and Y_clean
        ! X_clean :: X_wip without -1 entries
        ! Y_clean :: Y_wip without -1 entries
        ! summ :: Sum of integer products
        ! i_dum :: Integer for debugging loops
        ! enable_instructions :: Ask the user if they want to enable do and dont instructions | It is then transformed into another purpose for the instruct FSM
        ! instruct_state :: General state for do_state and dont_state

        ! -----------------------------------------------------------------------------------------------

        CHARACTER(LEN=1) :: file_char
        CHARACTER(LEN=3) :: X_char, Y_char
        ! file_char :: One character of filename at a time
        ! X_char :: Integer X, as character, of mul(X,Y). Has 1 to 3 digits
        ! Y_char :: Integer Y, as character, of mul(X,Y). Has 1 to 3 digits

        ! -----------------------------------------------------------------------------------------------

        INTEGER, PARAMETER :: unit = 11
        ! unit :: For reading filename

        ! -----------------------------------------------------------------------------------------------

        ! Read the file a first time to get number of rows
        OPEN(UNIT = unit, FILE = TRIM(filepath)//TRIM(filename), ACTION = "read", STATUS = "old")

        io = 0
        n_rows = 0
        DO WHILE(io == 0)
            n_rows = n_rows + 1
            READ(unit,'(A)', IOSTAT=io)
        END DO
        n_rows = n_rows - 1

        ! DEBUG
        WRITE(debug_unit,'(A,I2,A)')"File has ",n_rows," rows"
        WRITE(debug_unit,'(A)')""

        ! -----------------------------------------------------------------------------------------------

        ! Rewind the file to get individual characters

        REWIND(UNIT = unit) ! Go back to the start of the file

        ! Ask the user if they want do and dont instructions enabled
        WRITE(*,'(A)')"Do you want to enable do and dont instructions? (0: No, 1: Yes | Answer below)"
        READ(*,'(I1)')enable_instructions

        ! Initialize the sum of integer products
        summ = 0

        ! No instructions for do and donts
        IF(enable_instructions == 0) THEN

            ! Going over rows until we reach the last row
            row_count = 0
            state = 0
            DO WHILE(row_count <= n_rows)

                ! In a given row, read the file content, one character at a time
                io = 0
                DO WHILE(io == 0)

                    READ(unit,'(A)', ADVANCE='NO', IOSTAT=io)file_char

                    ! DEBUG
                    WRITE(debug_unit,'(A,I2)')"Character = "//file_char//" | io = ",io

                    ! Trigger to go through finite state machine (FSM)
                    IF(file_char == "m") state = 1

                    ! DEBUG
                    WRITE(debug_unit,'(A,I1)')"--> state = ",state

                    ! FSM for mul(X,Y)
                    SELECT CASE(state)
                        CASE(1) ! file_char == "m"

                            IF(file_char == "u") THEN

                                state = 2

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mu | state = ",state

                            ELSE IF(file_char == "m") THEN

                                state = 1

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> Gets into CASE(1) but nothing happens yet"

                            ELSE

                                state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                            END IF
                        
                        CASE(2) ! file_char == "u"

                            IF(file_char == "l") THEN

                                state = 3

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul | state = ",state

                            ELSE

                                state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                            END IF
                        
                        CASE(3) ! file_char == "l"

                            IF(file_char == "(") THEN

                                state = 4
                                i_x = 1 ! Getting ready for the case that we have an integer afterwards
                                X_wip(:) = -1 ! Use -1 to signal that this entry is not a digit of X

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul( | state = ",state
                                WRITE(debug_unit,'(A,I1)')"--> Puting X_wip integer at the start :: ",i_x
                                WRITE(debug_unit,'(A)')"--> Initial state of X_wip:"
                                DO i_dum = 1,3
                                    WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_wip(i_dum)
                                END DO

                            ELSE

                                state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                            END IF
                        
                        CASE(4) ! file_char == "(" | Parse X integer of 1 to 3 digits

                            ! --> Analyzing if its an integer
                            !io = 0
                            !A = "2"
                            !B = "B"
                            !READ(A,'(I1)',IOSTAT=io)c
                            !WRITE(*,'(I2)')io
                            !WRITE(*,'(I2)')c
                            !READ(B,'(I1)',IOSTAT=io)d
                            !WRITE(*,'(I9)')io
                            !WRITE(*,'(I2)')d
                            !--> A is a char of an int. io returns 0 and reads successfully as 2 for int c
                            !--> B is a char. io returns >0 and does not read anything onto int d
                            !--> Code compiles normally

                            CALL is_it_int(debug_unit, file_char, is_int, read_int)

                            IF(is_int == 0) THEN

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> It is NOT an integer. So what is it?"

                                IF(file_char == ",") THEN

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> It is a comma character. mul(X, | X has finished"

                                    ! We entered stage 4 and never left, reaching comma, means we have a valid X
                                    ! Next character, we go to validate Y

                                    state = 5
                                    i_y = 1 ! Getting ready for the case that we have an integer afterwards
                                    Y_wip(:) = -1 ! Use -1 to signal that this entry is not a digit of Y

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul( | state = ",state
                                    WRITE(debug_unit,'(A,I1)')"--> Puting Y_wip integer at the start :: ",i_y
                                    WRITE(debug_unit,'(A)')"--> Initial state of Y_wip:"
                                    DO i_dum = 1,3
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_wip(i_dum)
                                    END DO

                                ELSE

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> A different character | Sequence broken | state = ",state

                                END IF

                            ELSE

                                IF(i_x > 3) THEN ! Hard limit is 3 digits

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> X is longer than 3 digits | Sequence broken | state = ",state

                                END IF

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> It IS an integer, and after the parenthesis :: So it is X"

                                X_wip(i_x) = read_int

                                ! DEBUG
                                WRITE(debug_unit,'(A,I2)')"--> Store digit: ",X_wip(i_x)

                                i_x = i_x + 1

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Advance to store next digit :: ",i_x

                            END IF

                        CASE(5) ! file_char == "," beforehand in stage = 4 | Parse Y integer of 1 to 3 digits

                            CALL is_it_int(debug_unit, file_char, is_int, read_int)

                            IF(is_int == 0) THEN

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> It is NOT an integer. So what is it?"

                                IF(file_char == ")") THEN ! The valid sequence is complete

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> A closing parenthesis. mul(X,Y) is formed. Analyzing integers"

                                    ! ------- Forming valid X

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> Final state of X_wip:"
                                    DO i_dum = 1,3
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_wip(i_dum)
                                    END DO

                                    n_clean = COUNT( X_wip /= -1 )
                                    ALLOCATE(X_clean(n_clean))
                                    X_clean = PACK( X_wip, X_wip /= -1 )

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Length of actual X integer (number of elements of X_wip that are not -1) = ",n_clean
                                    WRITE(debug_unit,'(A)')"--> Result | X_clean elements:"
                                    DO i_dum = 1,n_clean
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_clean(i_dum)
                                    END DO

                                    X = 0
                                    DO i_x = 1,n_clean
                                        X = X*10 + X_clean(i_x)
                                    END DO

                                    DEALLOCATE(X_clean)

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I3)')"--> Final integer | X = ",X

                                    ! ------- Forming valid Y

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> Final state of Y_wip:"
                                    DO i_dum = 1,3
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_wip(i_dum)
                                    END DO
                                    
                                    n_clean = COUNT( Y_wip /= -1 )
                                    ALLOCATE(Y_clean(n_clean))
                                    Y_clean = PACK( Y_wip, Y_wip /= -1 )

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Length of actual Y integer (number of elements of Y_wip that are not -1) = ",n_clean
                                    WRITE(debug_unit,'(A)')"--> Result | Y_clean elements:"
                                    DO i_dum = 1,n_clean
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_clean(i_dum)
                                    END DO

                                    Y = 0
                                    DO i_y = 1,n_clean
                                        Y = Y*10 + Y_clean(i_y)
                                    END DO

                                    DEALLOCATE(Y_clean)

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I3)')"--> Final integer | Y = ",Y

                                    ! ------- Cumulative sum of integer products
                                    summ = summ + X*Y

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I12)')"--> Product between X and Y = ",X*Y
                                    WRITE(debug_unit,'(A,I12)')"--> Cummulative sum of X*Y | summ = ",summ

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence is over, back to the start | state = ",state

                                ELSE

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> A different character | Sequence broken | state = ",state

                                END IF

                            ELSE

                                IF(i_y > 3) THEN ! Hard limit is 3 digits

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Y is longer than 3 digits | Sequence broken | state = ",state

                                END IF

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> It IS an integer, and after the comma :: So it is Y"

                                Y_wip(i_y) = read_int

                                ! DEBUG
                                WRITE(debug_unit,'(A,I2)')"--> Store digit: ",Y_wip(i_y)

                                i_y = i_y + 1

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Advance to store next digit :: ",i_y

                            END IF

                    END SELECT

                    ! DEBUG
                    WRITE(debug_unit,'(A)')""

                END DO ! This loop always ends by reaching the end of the row; right now, file_char is a \n

                ! We move on to the next row
                row_count = row_count + 1

            END DO
        
        ! Instructions for do and donts
        ELSE IF(enable_instructions == 1) THEN

            ! enable_instructions will be used from now on. 0 to represent dont and 1 to represent do
            ! By default, mul instructions are enabled, i.e, enable_instructions = 1

            ! Going over rows until we reach the last row
            row_count = 0
            state = 0
            instruct_state = 0
            DO WHILE(row_count <= n_rows)

                ! In a given row, read the file content, one character at a time
                io = 0
                DO WHILE(io == 0)

                    READ(unit,'(A)', ADVANCE='NO', IOSTAT=io)file_char

                    ! DEBUG
                    WRITE(debug_unit,'(A,I2)')"Character = "//file_char//" | io = ",io

                    ! Trigger to go through finite state machine (FSM)
                    IF(file_char == "m") state = 1

                    ! Trigger to go through instructions finite state machine (FSM)
                    IF(file_char == "d") instruct_state = 1

                    ! DEBUG
                    WRITE(debug_unit,'(A,I1)')"--> state = ",state
                    WRITE(debug_unit,'(A,I1)')"--> instruct_state = ",instruct_state

                    ! FSM for do() and don't() instructions
                    SELECT CASE(instruct_state)
                        CASE(1) ! file_char == "d"

                            IF(file_char == "o") THEN

                                instruct_state = 2

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: do | instruct_state = ",instruct_state

                            ELSE IF(file_char == "d") THEN

                                instruct_state = 1

                                ! DEBUG
                                WRITE(debug_unit,'(A)')"--> Gets into INSTRUCT FSM CASE(1) but nothing happens yet"

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF

                        CASE(2) ! file_char == "o"

                            IF(file_char == "(") THEN

                                instruct_state = 4 ! Even instruct_state from here on out will be for do()

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: do( | Path to do instruct | Even states from now on | instruct_state = ",state
                            
                            ELSE IF(file_char == "n") THEN

                                instruct_state = 3 ! Odd instruct_state from here on out will be for don't()

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: don | Path to dont instruct | Odd states from now on | instruct_state = ",state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF
                        
                        CASE(3) ! Path to dont instruct | file_char == "n"

                            IF(file_char == "'") THEN

                                instruct_state = 5

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: don' | instruct_state = ",state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF

                        CASE(4) ! Path to do instruct | file_char == "(" 

                            IF(file_char == ")") THEN

                                enable_instructions = 1
                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1,A,I1)')"--> Sequence complete | do() | Enabling MUL FSM = ",enable_instructions," | Resetting instruct_state = ",instruct_state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF

                        CASE(5) ! Path to dont instruct | file_char == "'"

                            IF(file_char == "t") THEN

                                instruct_state = 7

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: don't | instruct_state = ",state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF
                        
                        CASE(7) ! Path to dont instruct | file_char == "t"

                            IF(file_char == "(") THEN

                                instruct_state = 9

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: don't( | instruct_state = ",state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF

                        CASE(9) ! Path to dont instruct | file_char == "("

                            IF(file_char == ")") THEN

                                enable_instructions = 0
                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1,A,I1)')"--> Sequence complete | don't() | Disabling MUL FSM = ",enable_instructions," | Resetting instruct_state = ",instruct_state

                            ELSE

                                instruct_state = 0

                                ! DEBUG
                                WRITE(debug_unit,'(A,I1)')"--> Sequence broken | instruct_state = ",instruct_state

                            END IF

                    END SELECT

                    ! Instructions are 1 if we had a do command
                    ! If they are 0, then we are in the dont section, so we ignore any mul(X,Y) we find
                    ! Until we get another do(), which will but instructions enabled again
                    IF(enable_instructions == 1) THEN

                        ! FSM for mul(X,Y)
                        SELECT CASE(state)
                            CASE(1) ! file_char == "m"

                                IF(file_char == "u") THEN

                                    state = 2

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mu | state = ",state

                                ELSE IF(file_char == "m") THEN

                                    state = 1

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> Gets into MUL FSM CASE(1) but nothing happens yet"

                                ELSE

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                                END IF
                            
                            CASE(2) ! file_char == "u"

                                IF(file_char == "l") THEN

                                    state = 3

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul | state = ",state

                                ELSE

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                                END IF
                            
                            CASE(3) ! file_char == "l"

                                IF(file_char == "(") THEN

                                    state = 4
                                    i_x = 1 ! Getting ready for the case that we have an integer afterwards
                                    X_wip(:) = -1 ! Use -1 to signal that this entry is not a digit of X

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul( | state = ",state
                                    WRITE(debug_unit,'(A,I1)')"--> Puting X_wip integer at the start :: ",i_x
                                    WRITE(debug_unit,'(A)')"--> Initial state of X_wip:"
                                    DO i_dum = 1,3
                                        WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_wip(i_dum)
                                    END DO

                                ELSE

                                    state = 0

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Sequence broken | state = ",state

                                END IF
                            
                            CASE(4) ! file_char == "(" | Parse X integer of 1 to 3 digits

                                ! --> Analyzing if its an integer
                                !io = 0
                                !A = "2"
                                !B = "B"
                                !READ(A,'(I1)',IOSTAT=io)c
                                !WRITE(*,'(I2)')io
                                !WRITE(*,'(I2)')c
                                !READ(B,'(I1)',IOSTAT=io)d
                                !WRITE(*,'(I9)')io
                                !WRITE(*,'(I2)')d
                                !--> A is a char of an int. io returns 0 and reads successfully as 2 for int c
                                !--> B is a char. io returns >0 and does not read anything onto int d
                                !--> Code compiles normally

                                CALL is_it_int(debug_unit, file_char, is_int, read_int)

                                IF(is_int == 0) THEN

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> It is NOT an integer. So what is it?"

                                    IF(file_char == ",") THEN

                                        ! DEBUG
                                        WRITE(debug_unit,'(A)')"--> It is a comma character. mul(X, | X has finished"

                                        ! We entered stage 4 and never left, reaching comma, means we have a valid X
                                        ! Next character, we go to validate Y

                                        state = 5
                                        i_y = 1 ! Getting ready for the case that we have an integer afterwards
                                        Y_wip(:) = -1 ! Use -1 to signal that this entry is not a digit of Y

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> Sequence is now :: mul( | state = ",state
                                        WRITE(debug_unit,'(A,I1)')"--> Puting Y_wip integer at the start :: ",i_y
                                        WRITE(debug_unit,'(A)')"--> Initial state of Y_wip:"
                                        DO i_dum = 1,3
                                            WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_wip(i_dum)
                                        END DO

                                    ELSE

                                        state = 0

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> A different character | Sequence broken | state = ",state

                                    END IF

                                ELSE

                                    IF(i_x > 3) THEN ! Hard limit is 3 digits

                                        state = 0

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> X is longer than 3 digits | Sequence broken | state = ",state

                                    END IF

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> It IS an integer, and after the parenthesis :: So it is X"

                                    X_wip(i_x) = read_int

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I2)')"--> Store digit: ",X_wip(i_x)

                                    i_x = i_x + 1

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Advance to store next digit :: ",i_x

                                END IF

                            CASE(5) ! file_char == "," beforehand in stage = 4 | Parse Y integer of 1 to 3 digits

                                CALL is_it_int(debug_unit, file_char, is_int, read_int)

                                IF(is_int == 0) THEN

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> It is NOT an integer. So what is it?"

                                    IF(file_char == ")") THEN ! The valid sequence is complete

                                        ! DEBUG
                                        WRITE(debug_unit,'(A)')"--> A closing parenthesis. mul(X,Y) is formed. Analyzing integers"

                                        ! ------- Forming valid X

                                        ! DEBUG
                                        WRITE(debug_unit,'(A)')"--> Final state of X_wip:"
                                        DO i_dum = 1,3
                                            WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_wip(i_dum)
                                        END DO

                                        n_clean = COUNT( X_wip /= -1 )
                                        ALLOCATE(X_clean(n_clean))
                                        X_clean = PACK( X_wip, X_wip /= -1 )

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> Length of actual X integer (number of elements of X_wip that are not -1) = ",n_clean
                                        WRITE(debug_unit,'(A)')"--> Result | X_clean elements:"
                                        DO i_dum = 1,n_clean
                                            WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",X_clean(i_dum)
                                        END DO

                                        X = 0
                                        DO i_x = 1,n_clean
                                            X = X*10 + X_clean(i_x)
                                        END DO

                                        DEALLOCATE(X_clean)

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I3)')"--> Final integer | X = ",X

                                        ! ------- Forming valid Y

                                        ! DEBUG
                                        WRITE(debug_unit,'(A)')"--> Final state of Y_wip:"
                                        DO i_dum = 1,3
                                            WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_wip(i_dum)
                                        END DO
                                        
                                        n_clean = COUNT( Y_wip /= -1 )
                                        ALLOCATE(Y_clean(n_clean))
                                        Y_clean = PACK( Y_wip, Y_wip /= -1 )

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> Length of actual Y integer (number of elements of Y_wip that are not -1) = ",n_clean
                                        WRITE(debug_unit,'(A)')"--> Result | Y_clean elements:"
                                        DO i_dum = 1,n_clean
                                            WRITE(debug_unit,'(A,I1,A,I2)')"--> (*) Index = ",i_dum," | Entry = ",Y_clean(i_dum)
                                        END DO

                                        Y = 0
                                        DO i_y = 1,n_clean
                                            Y = Y*10 + Y_clean(i_y)
                                        END DO

                                        DEALLOCATE(Y_clean)

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I3)')"--> Final integer | Y = ",Y

                                        ! ------- Cumulative sum of integer products
                                        summ = summ + X*Y

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I12)')"--> Product between X and Y = ",X*Y
                                        WRITE(debug_unit,'(A,I12)')"--> Cummulative sum of X*Y | summ = ",summ

                                        state = 0

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> Sequence is over, back to the start | state = ",state

                                    ELSE

                                        state = 0

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> A different character | Sequence broken | state = ",state

                                    END IF

                                ELSE

                                    IF(i_y > 3) THEN ! Hard limit is 3 digits

                                        state = 0

                                        ! DEBUG
                                        WRITE(debug_unit,'(A,I1)')"--> Y is longer than 3 digits | Sequence broken | state = ",state

                                    END IF

                                    ! DEBUG
                                    WRITE(debug_unit,'(A)')"--> It IS an integer, and after the comma :: So it is Y"

                                    Y_wip(i_y) = read_int

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I2)')"--> Store digit: ",Y_wip(i_y)

                                    i_y = i_y + 1

                                    ! DEBUG
                                    WRITE(debug_unit,'(A,I1)')"--> Advance to store next digit :: ",i_y

                                END IF

                        END SELECT

                    END IF ! End of instructions enabled

                    ! DEBUG
                    WRITE(debug_unit,'(A)')""

                END DO ! This loop always ends by reaching the end of the row; right now, file_char is a \n

                ! We move on to the next row
                row_count = row_count + 1

            END DO

        END IF

        ! DEBUG
        !WRITE(debug_unit,'(A,I2)')"[Out of the file loop] io = ",io
        !WRITE(debug_unit,'(A,I2,A)')"Finished after ",n_rows," rows"
        WRITE(debug_unit,'(A,I12)')"Sum of products = ",summ
        WRITE(*,'(A,I12)')"Sum of products = ",summ

        CLOSE(UNIT = unit) ! Close the file

    END SUBROUTINE read_file

    ! -----------------------------------------------------------------------------------------------

    SUBROUTINE is_it_int(debug_unit, c, res, read_int)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: debug_unit
        INTEGER, INTENT(INOUT) :: res, read_int
        CHARACTER(LEN=1), INTENT(IN) :: c

        INTEGER :: io_int

        ! DEBUG
        WRITE(debug_unit,'(A)')"--> is_it_int subroutine has been called | Arguments:"
        WRITE(debug_unit,'(A)')"--> (*) File character being studied :: "//c

        READ(c,'(I1)',IOSTAT=io_int)read_int

        WRITE(debug_unit,'(A,I2,A,I1)')"--> (*) Read status = ",io_int," | Integer read = ",read_int

        res = 0 ! By default, its not an int
        IF(io_int == 0) res = 1 ! Its an int

        WRITE(debug_unit,'(A,I1)')"--> (*) Conclusion, is it an integer? (0: No, 1: Yes) :: ",res

    END SUBROUTINE is_it_int

END PROGRAM main
