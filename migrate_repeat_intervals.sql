UPDATE task
SET repeat = CASE
        WHEN repeat LIKE 'Days %'
        OR repeat LIKE 'Weeks %'
        OR repeat LIKE 'Months %'
        OR repeat LIKE 'Years %' THEN 'ByUnitOfTime ' || repeat
        WHEN repeat LIKE 'OnWeekdays %' THEN TRIM(
            SUBSTRING(
                repeat
                from '.+ .+ '
            )
        )
        ELSE repeat
    END