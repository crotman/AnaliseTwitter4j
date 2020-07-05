package pack_x;
  
import importX.function;

class ClassX extends ClassY implements InterfX {
    private long fieldX;
    
    ClassX(int paramX, double paramY) {		
        int varX = function(paramX, paramY);		
        if (varX == 0){
            this.fieldX = 1;
        }
        else{
            this.fieldX = 0;
        }
    }
    @Override
    public int methodX(int paramW, Boolean paramZ)
    {
        if (paramZ)
            fieldX = paramW;
        else{
            fieldX = 0;
        }
        return paramW + this.fieldX;
     }
}  
