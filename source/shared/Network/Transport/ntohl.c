
int ntohl(int x){
       int ret;
       unsigned char* pr = (unsigned char*) &ret;
       unsigned char* px = (unsigned char*) &x;

       pr[0] =px[3]; pr[1] = px[2]; pr[2] = px[1]; pr[3] = px[0];
       return ret;
}





