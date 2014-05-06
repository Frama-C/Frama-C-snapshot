int x;

void main(){
    /*@ ensures \false; */
    /*@ assert \true; */
        x = 1;

        /*@ ensures \false; */
        { /*@ assert \true; */
            x = 1; }
}
