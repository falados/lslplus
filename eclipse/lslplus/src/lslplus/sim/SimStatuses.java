package lslplus.sim;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class SimStatuses {
    private static XStream xstream = new XStream(new DomDriver());
    
    public static class Message {
        public static final String INFO_LEVEL = "INFO";
        String time;
        String level;
        String source;
        String text;
        
        public String getText() { return text; }
        public String getTime() { return time; }
        public String getSource() { return source; }
        public String getLevel() { return level; }
    }
    public static class SimStatus { }
    public static class SimInfo extends SimStatus {
        private Message[] messages;
        
        public Message[] getMessages() {
            return messages;
        }
    }
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("sim-info", SimInfo.class); //$NON-NLS-1$
        xstream.alias("message", Message.class);        //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    public static SimStatus fromXML(String xml) {
        return (SimStatus) xstream.fromXML(xml);
    }
}
