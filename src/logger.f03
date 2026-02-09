module logger_module
    IMPLICIT NONE

    ! Define logger object
    type :: logger

        ! Log file file descriptor and file name
        integer :: log_fd

        ! Log on memory buffer
        character(len=4096) :: log_buffer
        integer :: buffer_size = 0

        contains
            procedure :: init_log
            procedure :: write_log
            procedure :: end_log

    end type logger

    contains

        subroutine init_log(this, path_in)
            IMPLICIT NONE

            ! Initialized logger instance for output
            class(logger), intent(inout) :: this
            character(len=*), intent(in) :: path_in
            integer :: return_stat

            ! Open file
            open(unit=this%log_fd, file=trim(adjustl(path_in)), action="READWRITE", iostat=return_stat)
            if (return_stat /= 0) then
                print *, "Failed to open log file."
                stop
            end if

            ! Set clear in-memory buffer to \0
            this%log_buffer(:) = achar(0)
            this%buffer_size = 0

        end subroutine init_log

        ! write_log
        subroutine write_log (this, s)

            ! Initialized instance 
            class(logger), intent(inout) :: this
            character(len=*), intent(in) :: s
            integer :: return_stat

            ! Check if buffer is overflow or not
            if ((this%buffer_size + len_trim(s)) > 4096) then

                ! If buffer overflow, flush to file system
                write(this%log_fd, *, iostat=return_stat) this%log_buffer
                if (return_stat /= 0) then
                    print *, "Failed to write to file."
                    stop
                end if 
                this%log_buffer(:) = achar(0)

                ! Reset Buffer size after flushed
                this%buffer_size = 0

            end if

            ! Write to buffer
            this%log_buffer(this%buffer_size + 1 : this%buffer_size + len_trim(s)) = s (1:len_trim(s))
            this%buffer_size = this%buffer_size + len_trim(s)


        end subroutine write_log

        ! end_log
        subroutine end_log (this)

            ! Declare object instance
            class(logger), intent(inout) :: this
            integer :: return_stat

            ! Flush buffer
            write(this%log_fd, *, iostat=return_stat) this%log_buffer
            if (return_stat /= 0) then
                print *, "Failed to write to file."
                stop
            end if

            ! Close file descriptor
            CLOSE(unit=this%log_fd)
            if (return_stat /= 0) then
                print *, "Failed to write to file."
                stop
            end if

        end subroutine end_log



end module logger_module