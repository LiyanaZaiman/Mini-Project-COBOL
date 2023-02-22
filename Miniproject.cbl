identification division.
program-id. Miniproject.

environment division.
input-output section.
file-control.
                select vaccination-file
                    assign to
            "C:\Users\Desktop\COBOL\vaccination.txt"
                    organization is line sequential.

configuration section.

data division.
file section.
        FD vaccination-file
        label records are standard.
01 vaccine-record.
    05 employee-id pic 9(3).
    05 name-of-employee pic x(20).
    05 vaccine-status pic x(5).
    05 date-first-dose pic 99x99x9999xxxxx.
    05 date-second-dose pic 99x99x9999xxxxx.

working-storage section.
01 ws-vaccination-file.
    05 quest-1 pic 9(3).
    05 quest-2 pic x(20).
    05 quest-3 pic x(5).
    05 quest-4 pic 99x99x9999xxxxx.
    05 quest-5 pic 99x99x9999xxxxx.

01 operation pic x.
01 yes-no pic x.
01 entry-ok pic x.
01 end-of-file pic x value "N".
01 employee-id-record pic 999999999999xxxxxxxx.
01 field-to-update pic 9.

*  	To display record
01 to-display-employee.
    05 Prompt-1 pic x(6) value "Name: ".
    05 Display-employee-name pic x(30).

    05 Prompt-2 pic x(16) value "Vaccine Status: ".
    05 Display-vaccine-status pic x(2).

    05 Prompt-3 pic x(17) value "First dose date: ".
    05 Display-first-dose pic 99x99x9999xxxxx.

    05 Prompt-4 pic x(18) value "Second dose date: ".
    05 Display-second-dose pic 99x99x9999xxxxx.

01 screen-lines pic 99.
01 A-Dummy pic x.

procedure division.
    perform operation-selection.
    if operation = "I"
        perform opening-procedure
        move "Y" to yes-no
        perform add-records until yes-no = "N"
        perform closing-procedure
    else
        if operation = "S"
                open input vaccination-file
                perform accept-search-input
                perform search-perform until end-of-file = "Y"
                perform closing-procedure
        else
            if operation = "U"
                    open input vaccination-file
                    display
                "---------------Employee Details--------------------"
                    perform display-records until end-of-file = "Y"
                    perform closing-procedure
                    open i-o vaccination-file
                    move "Y" to yes-no
                    perform update-records until yes-no = "N"
                    perform closing-procedure
                else
                    if operation = "D"
                        open i-o vaccination-file
                        display " "
                        perform display-records until end-of-file =
                            "Y"
                        perform closing-procedure
                        open i-o vaccination-file
                        move "Y" to yes-no
                        perform delete-records until yes-no = "N"
                        perform closing-procedure
                    else
                    if operation = "R"
                        display
                                "-------------------------------"
                        display
                        "FAMILY CO. Ltd. Employee Vaccine Status"
                        display space
                        open input vaccination-file
                        move ZEROES to screen-lines
                        move "N" to end-of-file
                        perform read-record
                        perform display-record until
                            end-of-file = "Y"
                        perform closing-procedure
                        else
                        display "Wrong input. Please try again"
                        end-if.

    goback.

    opening-procedure.
    open extend vaccination-file.

    operation-selection.
    display "WELCOME TO FAMILY CO. Ltd."
    display "Insert new employee record - I".
    display "Search employee record - S".
    display "Update employee vaccination information - U".
    display "Delete employee record - D".
    display "Full employee report - R".
    display " ".
    display "Which operation would you like to choose? :".
    accept operation.

    closing-procedure.
    close vaccination-file.

add-records.
    move "N" to entry-ok.
    perform get-fields until entry-ok = "Y".
    perform add-this-record.
    display "Vaccination status has been updated."

    perform go-again.

get-fields.
    move space to vaccine-record.
    display "What is your employee ID ".
    accept employee-id.
    display "What is your name?".
    accept name-of-employee.
    display "Vaccination Status C-Complete NC-Not Complete".
    accept vaccine-status.
    display
            "When did you get the first dose of vaccine xx/xx/xxxx ?".
    accept date-first-dose.
    display
            "When did you get the second dose of vaccine xx/xx/xxxx ?".
    accept date-second-dose.
    perform validate-fields.

    validate-fields.
    move "Y" to entry-ok.
    if name-of-employee = space
        display "NAME MUST BE ENTERED"
        move "N" to entry-ok.

    add-this-record.

    write vaccine-record.

go-again.
    display "Go again?".
    accept yes-no.
    if yes-no = "y"
        move "Y" to yes-no.
    if yes-no not = "Y"
        move "N" to yes-no.

update-records.
    display " ".
        display "Column Type:"
    display "1. Employee name".
    display "2. Employee vaccination status (C or NC)"
    display "3. Date of 1st dose"
    display "4. Date of 2nd dose"
    display " "
    display "Which column would you like to update?"
    accept field-to-update.
    display " "
    if field-to-update = 1
        display "Enter the employee name you want to update:"
        accept quest-2
        else
        if field-to-update = 2
                display
                    "Enter the employee ID you want to update:"
                accept quest-1
        else
            if field-to-update = 3
                display
                "Enter employee 1st dose date that you want to update:"
                    accept quest-4
                else
                    if field-to-update = 4
                        display
                "Enter employee 2nd dose date that you want to update:"
                        accept quest-5.
    move "N" to end-of-file.
    perform perform-update until end-of-file = "Y".
    if employee-id IS <= 000 OR employee-id IS > 999
            display "Wrong input. Please rerun again."
    else
        display "Vaccination status of has been updated".
    perform go-again.

perform-update.
    read vaccination-file next record
        at end
                move "Y" to end-of-file.
    if field-to-update IS = 1
        if quest-2 = name-of-employee
                display "Update it to?"
            accept quest-2
                move quest-2 to name-of-employee
                rewrite vaccine-record
                display " "
                display "Updated Record"
                display employee-id, name-of-employee,
                vaccine-status,
                    date-first-dose, date-second-dose
        end-if.
    if field-to-update IS = 2
        if quest-1 = employee-id
            if vaccine-status = "NC   "
                    move "C" to vaccine-status
                    if employee-id IS <= 000 OR employee-id IS > 999
                        display "Wrong input. Wrong input"
                    else
                        rewrite vaccine-record
                        display " "
                        display "Updated Record"
                        display employee-id, name-of-employee,
                            vaccine-status,
                            date-first-dose, date-second-dose
                    end-if
                else
                    move "NC" to vaccine-status
                    if employee-id IS <= 000 OR employee-id IS > 999
                        display "Wrong input. "
                    else
                        rewrite vaccine-record
                        display " "
                        display "Updated Record"
                        display employee-id, name-of-employee,
                            vaccine-status,
                            date-first-dose, date-second-dose
                    end-if
                end-if
        end-if.
    if field-to-update IS = 3
        if quest-4 = date-first-dose
                display "Update it to?"
                accept quest-4
                move quest-4 to date-first-dose
                rewrite vaccine-record
                display " "
                display "Updated Record"
                display employee-id, name-of-employee,
                    vaccine-status,
                    date-first-dose, date-second-dose

        end-if.
    if field-to-update IS = 4
        if quest-5 = date-second-dose
                display "Update it to?"
                accept quest-5
                move quest-5 to date-second-dose
                display " "
                rewrite vaccine-record
                display "Updated Record"
                display employee-id, name-of-employee,
                    vaccine-status,
                    date-first-dose, date-second-dose
        end-if
    end-if.

delete-records.
    display " "
    display
            "Type in the employee ID to delete its vaccination status:"
    accept quest-1.
    move "N" to end-of-file.
    perform perform-delete until end-of-file = "Y".
    if employee-id IS NOT = quest-1
        display "Data for " quest-1 " has been deleted"
    else
        display "Wrong input. Please rerun again.".
    perform go-again.

perform-delete.
    read vaccination-file next record
        at end
                move "Y" to end-of-file.
    if employee-id = quest-1
        move spaces to vaccine-record
        rewrite vaccine-record
        end-if.

read-record.
    read vaccination-file next record
        at end
                move "Y" to
                    end-of-file.

display-record.
    perform display-fields.
    perform read-record.

    display-records.
    read vaccination-file next record
        at end
                move "Y" to end-of-file.
    move vaccine-record to ws-vaccination-file.
    display ws-vaccination-file.

search-perform.
    read vaccination-file next record
    if employee-id = employee-id-record
        display "Record found : "
                employee-id,
                name-of-employee, vaccine-status,
                date-first-dose, date-second-dose
    else
        display "Employee record not found".

    accept-search-input.
    display
            "Employee IC number : ".
    accept employee-id-record.

*      	display all record
display-fields.
    if screen-lines = 15
        perform press-enter.
    display " ".
    move name-of-employee to Display-employee-name.
    move vaccine-status to Display-vaccine-status.
    move date-first-dose to Display-first-dose.
    move date-second-dose to Display-second-dose.
    display to-display-employee.
    add 1 to screen-lines.

press-enter.
        display "PRESS ENTER  to continue...".
    accept A-Dummy.
    move zeroes to screen-lines.
end program Miniproject.
