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
	CHARACTER(len=100000000)::file_path_input
	CHARACTER(len=100000000)::file_path_output

! Read in path for text file input and output
	CALL get_command_argument(1,file_path_input)
	CALL get_command_argument(2,file_path_output)

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
! NEED TO MAKE IT SO IF SAME ELEMENT THEN GOES TO NEXT ONE
	DO i=1,height
		DO j=i+1,height
			IF (arr(i,1)>arr(j,1)) THEN
				temp_arr_1=arr(i,:)
				temp_arr_2=arr(j,:)
				arr(i,:)=temp_arr_2
				arr(j,:)=temp_arr_1
			END IF
		END DO
	END DO


! Printing out the Array
	DO i=1,height
		print *,(arr(i,j),j=1,width)
		WRITE(2,*) (arr(i,j),j=1,width)
		!DO j=1,width
		!print*,arr(i,j)
		!END DO
	END DO

! Writing array to output.txt

END PROGRAM assign3
