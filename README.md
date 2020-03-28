# edificios

## Instructions
The program (ciao) can be run with: ``` ciao ```

- Load a database, at the beginning or each time the program changes: ``` ?- consult('*.pl'). \n %OR \n ?- [db]. ```

- Exit the program: ``` ?- halt. ```

- Print a message with a new line: ``` ?- write('Hello World'), nl. ```

- Get all possible solutions to a fact or predicate: ``` ?- <predicate>(X <, Y>). \nl % cicle with ; ```

- Query two things at the same time: ``` ?- query1(X), query2(X). ```

- To generate documentation ``` lpdoc -t pdf codigo.pl ```
