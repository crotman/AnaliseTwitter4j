package pack_x;
  
import importX.function;

class ClassX extends ClassY implements InterfX {
    private long fieldX;
    
    ClassX(int paramX, double paramY) {		
        int varX = function(paramX, paramY);		
        if (varX == 0)
        {
            this.fieldX = 1;
        }			
        else{
            this.fieldX = 0;
        }
    }
    @Override
    public int methodZ(int paramW, Boolean paramZ)
    {
		if(paramZ == 0){
            if (paramZ)
                fieldX = paramW;
            else{
                fieldX = 0;
            }
		}	
	    else{
		    fieldX = 1;
		}
        return paramW + this.fieldX;
     }
}  
