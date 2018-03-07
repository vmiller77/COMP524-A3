PROGRAM assign3
IMPLICIT NONE

! Type declarations
	INTEGER :: height
	INTEGER :: width
	INTEGER :: i = 1
	INTEGER :: j = 1
	INTEGER :: k
	INTEGER :: temp
	INTEGER, dimension(2)::dim
	INTEGER, ALLOCATABLE,dimension(:,:) :: arr
	INTEGER, ALLOCATABLE,dimension(:) :: temp_arr_1
	INTEGER, ALLOCATABLE,dimension(:) :: temp_arr_2
	INTEGER :: done

	! Variables for input/output files and length
	INTEGER arglen1
	INTEGER arglen2
	CHARACTER(len=:), ALLOCATABLE :: file_path_input
	CHARACTER(len=:), ALLOCATABLE :: file_path_output

	!Getting argument lengths
	CALL GET_COMMAND_ARGUMENT(1,length=arglen1)
	CALL GET_COMMAND_ARGUMENT(2,length=arglen2)

	!Allocating lengths to file path variables
	ALLOCATE(CHARACTER(arglen1)::file_path_input)
	ALLOCATE(CHARACTER(arglen2)::file_path_output)

	!Storing paths into file path variables
	CALL GET_COMMAND_ARGUMENT(1,file_path_input)
	CALL GET_COMMAND_ARGUMENT(2,file_path_output)

! Getting txt file input and output
	OPEN(UNIT=1,FILE=file_path_input,STATUS='old',ACTION='read')
	OPEN(UNIT=2,FILE=file_path_output,STATUS='new',ACTION='write')

! Read in height and width
	READ(1,*), dim
	height=dim(1)
	width=dim(2)

! Allocate correct dimensions to array
	ALLOCATE(arr(height,width))
	ALLOCATE(temp_arr_1(width))
	ALLOCATE(temp_arr_2(width))

! Read until end of column
	DO k=1,height
		READ(1,*),(arr(k,j),j=1,width)
	END DO

! Sort array in rows
	DO i=1,height
		DO j=1,width
			DO k=j+1,width
				IF (arr(i,j)>arr(i,k)) THEN
					temp=arr(i,j)
					arr(i,j)=arr(i,k)
					arr(i,k)=temp
				END IF
			END DO
		END DO
	END DO

! Sort array in columns
	done=0
	! Go through the element of each row
		!Go through the rows of the arrays
		DO i=1,height
			DO j=i+1,height
				!Compare the kth element of each row
				done=0
				DO k=1,width
					IF(done==0) THEN
						IF (arr(i,k)>arr(j,k)) THEN
							temp_arr_1=arr(i,:)
							temp_arr_2=arr(j,:)
							arr(i,:)=temp_arr_2
							arr(j,:)=temp_arr_1
							done=1
						ELSE IF (arr(i,k)<arr(j,k)) THEN
							done=1
						END IF
					END IF
				END DO
			END DO
		END DO



! Printing out the Array
	DO i=1,height
!		print *,(arr(i,j),j=1,width)
		WRITE(2,*) (arr(i,j),j=1,width)
	END DO

END PROGRAM assign3
