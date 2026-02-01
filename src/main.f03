program main
    use multiarm_bandit_module

    type(multiarm_bandit) :: foo
    allocate(foo%weight(5))
    foo%weight(1) = 10
    print *,  foo%weight(1)

end program main