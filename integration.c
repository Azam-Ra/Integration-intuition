#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#define Pi 3.141592653589
#define e 2.7182818284

// Print Menu
void Menu(){
    printf("Choose value_p of what type of function you want to find:");
    printf("\n1. Simple polynomial");
    printf("\n2. Mathematical functions");
}

// Different notation for polynomial
void Notation_polynomial(){
    printf("\n\nRange of available operations: +, -, /, ^.");
    printf("\nUse x for an independent variable and do not leave spaces between operations.");
    printf("\nEnter a polynomial function: ");
}

// Different notation for mathematical functions
void Notation_mathematical_function(){
    printf("\n\nList of all available mathematical functions: ");
    printf("\n  (The notation: input the value_p of bounds in degrees, not in radians. sinx - example).");
    printf("\n  - sin, cos, tg, ctg, arcsin, arccos, arctg");
    printf("\n  (The notation: log(a)x is an example, where a is a base).");
    printf("\n  - log(a)x, log(Pi)x~(base Pi = 3.14159), lgx~(base 10), lnx~(base e)");
    printf("\n  (The notation: a,k is integer value_p)");
    printf("\n  - e^ax, a^kx");
    printf("\nUse x for an independent variable and do not leave spaces between operations.");
    printf("\nYou can only find a value_p of addition and subtraction of these mathematical functions.");
    printf("\nEnter a mathematical function: ");
}

typedef struct {  // specific structure to bond precise and approximate variables
    double precise, approximate;
} answer;

char* Get_string(){  // in order to not limit user in his input, we do not use scanf or gets
    char newchar;
    char *str = (char*) malloc(1*sizeof(char));
    str[0] = '\0';

    while (scanf("%c", &newchar) == 1 && newchar != '\n'){
        char *dup = (char*) malloc((strlen(str) + 2) * sizeof(char));  // +2: 1 for newchar, 1 for '\0'
        strcpy(dup, str);
        dup[strlen(str)] = newchar;
        dup[strlen(str) + 1] = '\0';
        str = dup;
    }

    return str;
}

double module(double x){  // when I use abs() the programm doesn't run
    if (x < 0){
        return -x;
    }
    return x;
}

/*in order to keep code clean we use function with change in base*/
double Logarithm_precise(double base, double l, double r){  // finding precise value of logarithm
    double value = 0;

    value = (log(r)/log(base))*r - r/log(base) - (log(l)/log(base))*l + l/log(base);

    return value;
}

double Logarithm_approximate(double base, double l, double r, double dx){  // finding approximate value of logarithm
    double value = 0;

    while (l < r){
        double rectangle, height1, height2;
        height1 = log(l)/log(base);
        l += dx;
        height2 = log(l)/log(base);
        if (module(height1) < module(height2)){
            rectangle = dx * height1;
        }
        else{
            rectangle = dx * height2;
        }
        value += rectangle;
    }

    return value;
}

// converts strings into double values
double Conv(char string[]){
    if (string[0] == '\0'){
        return 1.0;
    }

    double n = 0, temp = strlen(string) - 1;
    for (int i = 0; i < strlen(string); i++){
        n += (string[i] - 48) * pow(10, temp);
        temp--;
    }

    return n;
}

// for finding each term in the function
char *Subs(char string[], int start, int end){
    int length = end - start;
    char *dup = (char*) malloc((length + 1) * sizeof(char));
    memcpy(dup, string + start, length * sizeof(char));
    dup[length] = '\0';

    return dup;
}

// to convert Pi form to pure radians
double Conv_to_radians(char *string){
    int sign = 0;  // to check if the value is negative or positive
    if (string[0] == '-'){
        string++;
        sign--;
    }

    int pi_place = -1;
    while(string[pi_place + 1] != 'P' && string[pi_place + 1] != '\0'){
        ++pi_place;
    }
    ++pi_place;

    if (string[pi_place] == '\0'){  // when there is no Pi, that's just number for arithmetic functions
        if (sign == -1){  // for negative input
            return -Conv(string);
        }
        return Conv(string);
    }
    else{  // when there is a Pi, we convert string into double radians
        int div = -1;
        for (int j = 0; j < strlen(string); j++){
            if (string[j] == '/'){
                div = j;
                break;
            }
        }

        double out_val = Conv(Subs(string, 0, pi_place));

        if (div == -1){  // when there is no division
            if (sign == -1){
                return -(out_val * Pi);
            }
            return out_val * Pi;
        }
        else{
            double divider = Conv(Subs(string, div + 1, strlen(string)));
            if (sign == -1){  // for negative input
                return -(out_val * Pi) / divider;
            }
            return (out_val * Pi) / divider;
        }
    }
}

void Simple_polynomial(double l_bound, double r_bound, double dx, char *function, double *precise, double *approximate, FILE *fptr){

    if (function[0] == '\0'){  //end of the recursion
        return;
    }

    double value_a = 0;  // an approximate value of each termp in polynomial
    double *rectangle = (double*) malloc(sizeof(double));  // the value of each rectangle with weight dx
    double value_p = 0;  // a precise value of each term in polynomial

    double *temp = (double*) malloc(sizeof(double));
    *temp = l_bound;

    int x_factor = -1;  // to check if there is x in each term
    char *term_string;
    int i;  // we will need it in the recursion

    if (function[0] == '-' || function[0] == '+'){
        i = 1;  // 1 is the index of the start of the string including - or +

        while (function[i] != '-' && function[i] != '+' && function[i] != '\0'){  //finding where a term ends
            i++;
        }

        term_string = Subs(function, 1, i);  //extracting the term out of function
    }
    else{  //only for the start of the of the whole function
        i = 0;  //0 is the index of the start of the string

        while (function[i] != '-' && function[i] != '+' && function[i] != '\0'){  //finding where a term ends
            i++;
        }

        term_string = Subs(function, 0, i);  //extracting the term out of function without a sign
    }

    for (int j = 0; j < strlen(term_string); j++){  //finding x in a term
        if (term_string[j] == 'x'){
            x_factor = j;
            break;
        }
    }

    if (x_factor == -1){  //if there is no x, it can only be an integer value
        value_p = Conv(term_string) * r_bound - Conv(term_string) * l_bound;
        // in this case area under the graph is rectangle, and we can precisely find its area through fividing it with many rectangles
        value_a = value_p;
    }
    else{  //finding value_p of a term considering all the possible combinations
        int exp = -1, div1 = -1, div2 = -1;
        if (term_string[x_factor + 1] == '^'){  //checking if there is ^ after x
            exp = x_factor + 1;
        }
        for (int j = 0; j < strlen(term_string); j++){  //checking for / before or after x
            if (term_string[j] == '/' && j < x_factor){
                div1 = j;
            }
            else if (term_string[j] == '/' && j > x_factor){
                div2 = j;
            }
        }

        /*we use different formulas for different forms, however the logic is the dame*/
        if (div1 == -1 && div2 == -1 && exp == -1){  //in case when the term is ax, where a is any integer number
            value_p = Conv(Subs(term_string, 0, x_factor)) * ((pow(r_bound, 2) - pow(l_bound, 2)) / 2);

            while (*temp < r_bound){
                double height1, height2;  // we need only 2 heights for comparison and we choose the smaller one
                height1 = Conv(Subs(term_string, 0, x_factor)) * *temp;  // the formula changes for every different form
                *temp += dx;
                height2 = Conv(Subs(term_string, 0, x_factor)) * *temp;
                if (module(height1) < module(height2)){  // we check for the smaller height in order not to exceed the graph of funciton
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        else if (div1 == -1 && div2 == -1 && exp > 0){  //in ax^k
            double e_d = Conv(Subs(term_string, x_factor + 2, strlen(term_string))) + 1;
            value_p = Conv(Subs(term_string, 0, x_factor)) * ((pow(r_bound, e_d) - pow(l_bound, e_d)) / e_d);

            double exp = Conv(Subs(term_string, x_factor + 2, strlen(term_string)));
            while (*temp < r_bound){
                double height1, height2;
                height1 = Conv(Subs(term_string, 0, x_factor)) * pow(*temp, exp);
                *temp += dx;
                height2 = Conv(Subs(term_string, 0, x_factor)) * pow(*temp, exp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        /*we do not check for div2 because if div1 > 0 -> div2 == -1 and vise versa, because smart user enters simplified expression*/
        else if (div2 > 0 && exp == -1){  // in ax/n, where a, n are integer values
            double out_val = Conv(Subs(term_string, 0, x_factor)) / Conv(Subs(term_string, div2 + 1, strlen(term_string)));  // constant value that does not change
            value_p = ((pow(r_bound, 2) - pow(l_bound, 2)) / 2) * out_val;

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * *temp;
                *temp += dx;
                height2 = out_val * *temp;
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        else if (div2 > 0 && exp > 0){  // in ax^k/n case
            double e_d = Conv(Subs(term_string, x_factor + 2, div2)) + 1;  // in this case the values of exp and divider are the same
            double out_val = Conv(Subs(term_string, 0, x_factor)) / Conv(Subs(term_string, div2 + 1, strlen(term_string)));
            value_p = ((pow(r_bound, e_d) - pow(l_bound, e_d)) / e_d) * out_val;

            double exp = Conv(Subs(term_string, x_factor + 2, div2));
            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * pow(*temp, exp);
                *temp += dx;
                height2 = out_val * pow(*temp, exp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        else if (div1 > 0 && exp > 0){  // in a/nx^k case
            if (r_bound >= 0 && l_bound <= 0){  // it is reciprocal function therefore range cannot contain 0
                fprintf(fptr, "You have entered reciprocal function and it is undefinable in this range.\n");
                *precise = 0;
                *approximate = 0;
                return;
            }

            double e_d = -Conv(Subs(term_string, x_factor + 2, strlen(term_string))) + 1;  // negative because the x is in denumenator
            double out_val = Conv(Subs(term_string, 0, div1)) / Conv(Subs(term_string, div1 + 1, x_factor));
            value_p = ((pow(r_bound, e_d) - pow(l_bound, e_d)) / e_d) * out_val;

            double exp = Conv(Subs(term_string, x_factor + 2, strlen(term_string)));
            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * pow(*temp, -exp);  // 1 / pow(a, b) == pow(a, -b)
                *temp += dx;
                height2 = out_val * pow(*temp, -exp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        else if (div1 > 0 && exp == -1){  //the case when we have to implement ln|x|
            if (r_bound >= 0 && l_bound <= 0){  // it is reciprocal function therefore range cannot contain 0
                fprintf(fptr, "You have entered reciprocal function and it is undefinable in this range.\n");
                *precise = 0;
                *approximate = 0;
                return;
            }

            double out_val = Conv(Subs(term_string, 0, div1)) / Conv(Subs(term_string, div1 + 1, x_factor));
            value_p = out_val * (log(abs(r_bound)) - log(abs(l_bound)));

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * (1 / *temp);
                *temp += dx;
                height2 = out_val * (1 / *temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
        }

        else{
            fprintf(fptr,"Oops! We cannot compute it yet:(\nMaybe recheck the function for mistakes.\n");
            *precise = 0;
            *approximate = 0;
            return;
        }
    }

    if (function[0] == '-'){  // by a sign we decide either to add or subtract
        *precise -= value_p;
        *approximate -= value_a;
    }
    else{
        *precise += value_p;
        *approximate += value_a;
    }

    free(temp);

    Simple_polynomial(l_bound, r_bound, dx, function + i, precise, approximate, fptr);  //recursion
}

void Mathematical_function(double l_bound, double r_bound, double dx, char *function, double *precise, double *approximate, FILE *fptr){

    if (function[0] == '\0'){  //end of the recursion
        return;
    }

    double value_a = 0;  // an approximate value of each term in polynomial
    double *rectangle = (double*) malloc(sizeof(double));  // the value of each rectangle with weight dx
    double value_p = 0;  // a precise value of each term in polynomial

    double *temp = (double*) malloc(sizeof(double));
    *temp = l_bound;

    char *term_string;  // !!!!!!!!!!!!!!!!!!!!-----------FREE IT-----------!!!!!!!!!!!!!!!!!!!!
    int i;
    int x_factor = -1;

    if (function[0] == '-' || function[0] == '+'){
        i = 1;  //1 is the index of the start of the string including - or +

        while (function[i] != '-' && function[i] != '+' && function[i] != '\0'){  //finding where a term ends
            i++;
        }

        term_string = Subs(function, 1, i);  //extracting the term out of function
    }
    else{  //only for the start of the of the whole function
        i = 0;  //0 is the index of the start of the string

        while (function[i] != '-' && function[i] != '+' && function[i] != '\0'){  //finding where a term ends
            i++;
        }

        term_string = Subs(function, 0, i);  //extracting the term out of function
    }

    int sign_mf = -1;
    for (int j = 0; j < strlen(term_string); j++){  // to check what type of mathematic function it is
        if (term_string[j] > '9' || term_string[j] < '0' || term_string[j] == '^' || term_string[j] == '*'){
            sign_mf = j;
            break;
        }
    }

    for (int j = 0; j < strlen(term_string); j++){  //finding x in a term
        if (term_string[j] == 'x'){
            x_factor = j;
            break;
        }
    }

    double out_val = Conv(Subs(term_string, 0, sign_mf));

    // it is easier to group mathematical funcitons with the first letter of function
    // again we use different formulas for different functions
    switch(term_string[sign_mf]){
        case 's':  // for sin
            value_p = out_val * (-cos(r_bound) + cos(l_bound));

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * sin(*temp);
                *temp += dx;
                height2 = out_val * sin(*temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;  // we will change the sign of an area in the end
            }

            break;
        case 't':  // for tg
        {
            value_p = out_val * (-log(module(cos(r_bound))) + log(module(cos(l_bound))));

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * tan(*temp);
                *temp += dx;
                height2 = out_val * tan(*temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }

            break;
        }
        case 'c':
            if (term_string[sign_mf + 1] == 'o'){ // for cos
                value_p = out_val * (sin(r_bound) - sin(l_bound));

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * cos(*temp);
                *temp += dx;
                height2 = out_val * cos(*temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
            break;
            }

            else if (term_string[sign_mf + 1] == 't'){  // for ctg
                value_p = out_val * (log(module(sin(r_bound))) - log(module(sin(l_bound))));

                while (*temp < r_bound){
                    double height1, height2;
                    height1 = out_val * cos(*temp)/sin(*temp);
                    *temp += dx;
                    height2 = out_val * cos(*temp)/sin(*temp);
                    if (module(height1) < module(height2)){
                        *rectangle = dx * height1;
                    }
                    else{
                        *rectangle = dx * height2;
                    }
                    value_a += *rectangle;
                }

                break;
            }
            else{
                fprintf(fptr, "Oops! You made a mistake typing trigonometric cos or ctg functions.\n");
                *precise = 0;
                *approximate = 0;
                break;
            }
        case 'l':  // for logarithms
            if (l_bound <= 0 || r_bound <= 0){
                fprintf(fptr, "The bounds cannot be negative or zero when you want to calculate logarithm!\n");
                *precise = 0;
                *approximate = 0;
                return;
            }

            if (term_string[sign_mf + 1] == 'n'){  // for ln
                value_p = out_val * Logarithm_precise(e, l_bound, r_bound);
                value_a = out_val * Logarithm_approximate(e, l_bound, r_bound, dx);
                break;
            }
            else if (term_string[sign_mf + 1] == 'g'){  // for lg
                value_p = out_val * Logarithm_precise(10, l_bound, r_bound);
                value_a = out_val * Logarithm_approximate(10, l_bound, r_bound, dx);
                break;
            }
            else if (term_string[sign_mf + 3] == 'P'){  // for logPi
                value_p = out_val * Logarithm_precise(Pi, l_bound, r_bound);
                value_a = out_val * Logarithm_approximate(Pi, l_bound, r_bound, dx);
                break;
            }
            else if (term_string[sign_mf + 3] == '('){  // for log(a)
                int br[] = {-1, -1};  // for finding brackets ()
                for (int j = 0; j < strlen(term_string); j++){  // for opening bracket
                    if (term_string[j] == '('){
                        br[0] = j;
                        while (j < strlen(term_string)){
                            ++j;
                            if (term_string[j] == ')'){  // for closing bracket
                                br[1] = j;
                                break;
                            }
                        }
                    }
                }

                value_p = out_val * Logarithm_precise(Conv(Subs(term_string, br[0] + 1, br[1])), l_bound, r_bound);
                value_a = out_val * Logarithm_approximate(Conv(Subs(term_string, br[0] + 1, br[1])), l_bound, r_bound, dx);

                break;
            }
            else{
                fprintf(fptr, "Oops! You made a mistake typing logarithm functions.\n");
                *precise = 0;
                *approximate = 0;
                break;
            }
        case 'e':  // for ae^kx
        {
            double e_d = Conv(Subs(term_string, sign_mf + 2, x_factor));  // the integer in exponential
            value_p = out_val * ((pow(e, e_d * r_bound) - pow(e, e_d * l_bound))/e_d);

            while (*temp < r_bound){
                double height1, height2;
                height1 = out_val * pow(e, e_d * *temp);  // e_d because it does not change in an integral of e^kx
                *temp += dx;
                height2 = out_val * pow(e, e_d * *temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }

            break;
        }
        case '^':  // for a^kx
        {
            double e_d = Conv(Subs(term_string, sign_mf + 1, x_factor));
            value_p = (pow(out_val, e_d * r_bound) - pow(out_val, e_d * l_bound)) / (e_d * log(out_val));

            while (*temp < r_bound){
                double height1, height2;
                height1 = pow(out_val, e_d * *temp);
                *temp += dx;
                height2 = pow(out_val, e_d * *temp);
                if (module(height1) < module(height2)){
                    *rectangle = dx * height1;
                }
                else{
                    *rectangle = dx * height2;
                }
                value_a += *rectangle;
            }
            break;
        }
        default:
            fprintf(fptr,"Oops! We cannot compute it yet:(\nMaybe recheck the function for mistakes.\n");
    }

    if (function[0] == '-'){
        *precise -= value_p;
        *approximate -= value_a;
    }
    else{
        *precise += value_p;
        *approximate += value_a;
    }

    free(temp);

    Mathematical_function(l_bound, r_bound, dx, function + i, precise, approximate, fptr);
}

int main()
{
    Menu();
    printf("\nWhat do you want to calculate?\n");
    double choice = Conv(Get_string());  // we do not just get int value because \n is transformed to function = Get_string()

    double l_bound, r_bound;  // range of the function
    char *bounds;  // to enter range in radiand

    double dx;

    FILE *fptr;
    fptr = fopen("C:\\Basics of programming\\Integral_calculator.txt","w");

    if(fptr == NULL)  // for exceptions
    {
      printf("Error!");
      exit(1);
    }

    char *function;
    answer output;
    output.precise = 0, output.approximate = 0;
    double difference = 0;  // difference in precise and approxximatevalues

    switch((int) choice)
    {
        case 1:
            Notation_polynomial();
            function = Get_string();

            printf("\nUse any integer value.");
            printf("\nSet the left and right bounds: ");
            scanf("%lf %lf", &l_bound, &r_bound);
            if (l_bound == r_bound){
                fprintf(fptr, "\nYou have entered equal bounds. Therefore there cannot exist any area between them.");
                fprintf(fptr, "precise: %lf\napproximate: %lf", output.precise, output.approximate);
                break;
            }

            printf("\n(Remark: the smaller the value of dx is, the more precise value of an area you get.)");
            printf("\nEnter the value of dx: ");
            scanf("%lf", &dx);

            Simple_polynomial(l_bound, r_bound, dx, function, &output.precise, &output.approximate, fptr);
            fprintf(fptr, "precise: %lf\napproximate: %lf", output.precise, output.approximate);

            difference = module(output.precise - output.approximate);  // to show absolute value of difference
            fprintf(fptr, "\nThe difference between numercial and traditional ways of integraion is: %lf", difference);

            free(function);

            break;

        case 2:
        {
            Notation_mathematical_function();
            function = Get_string();

            printf("\nIf yo want to count trigonometric function or mix, enter the values in radians. For 3.14 value type Pi. E.g. [Pi/4 3Pi].");
            printf("\nSet the left and right bounds: ");
            bounds = Get_string();  // to enter bounds in Pi form

            int space = -1;
            while (bounds[space + 1] != ' ' && space + 1 < strlen(bounds)){  // to separate bounds
                ++space;
            }
            ++space;

            l_bound = Conv_to_radians(Subs(bounds, 0, space));
            r_bound = Conv_to_radians(Subs(bounds, space + 1, strlen(bounds)));
            if (l_bound == r_bound){
                fprintf(fptr, "\nYou have entered equal bounds. Therefore there cannot exist any area between them.");
                fprintf(fptr, "precise: %lf\napproximate: %lf", output.precise, output.approximate);
                break;
            }

            printf("\n(Remark: the smaller the value of dx is, the more precise value of an area you get.)");
            printf("\nEnter the value of dx: ");
            scanf("%lf", &dx);

            Mathematical_function(l_bound, r_bound, dx, function, &output.precise, &output.approximate, fptr);
            fprintf(fptr, "precise: %lf\napproximate: %lf", output.precise, output.approximate);

            difference = module(output.precise - output.approximate);  // we need to show absolute value of difference
            fprintf(fptr, "\nThe difference between numerical and traditional ways of integration is: %lf", difference);

            free(bounds);
            free(function);

            break;
        }
        default:
            fprintf(fptr, "\nOops! Invalid choice.");
    }

    return 0;
}
