package lslplus;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslExpressionValidator {
    private static class Expr {
        public String type;
        public String text;
        
        public Expr(String type, String text) {
            this.type = type;
            this.text = text;
        }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    
    static {
        xstream.alias("expression", Expr.class); //$NON-NLS-1$
    }
    
    public static String validateExpression(String type, String expression) {
        Expr e = new Expr(type,expression);
        String xml = xstream.toXML(e);
        return LslPlusPlugin.validateExpression(xml);
    }
}
