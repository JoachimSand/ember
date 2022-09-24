int main(){
    int (*(*fp_arr[3][9])[15])();
    int a = 1;
    int b = 4 + 2/a++;
    if(b)
        a = 5;
     else 
        a = 8;
}
