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
    
    public static interface NameKeyType {
        public String getName();
        public String getKey();
        public String getCombinedRepresentation();
    }
    
    public static class SimPrim implements NameKeyType {
        private String key;
        private String name;
        
        public String getKey() { return key; }
        public String getName() { return name; }
        
        public String toString() {
            return name + " - " + key;
        }
        
        public String getCombinedRepresentation() {
            return toString();
        }
    }
    
    public static class SimAvatar implements NameKeyType {
        private String key;
        private String name;
        
        public String getKey() { return key; }
        public String getName() { return name; }
        public String getCombinedRepresentation() {
            
            return toString();
        }
        
        public String toString() {
            return name + " - " + key;
        }
    }
    
    public static class SimState {
        private SimPrim[] prims;
        private SimAvatar[] avatars;
        
        public SimPrim[] getPrims() { return prims; }
        public SimAvatar[] getAvatars() { return avatars; }
    }
    
    public static class SimStatus {
        private Message[] messages;
        private SimState state;
        public Message[] getMessages() {
            return messages;
        }
        
        public SimState getState() {
            return state;
            
        }
    }
    
    public static class SimInfo extends SimStatus {
    }

    public static class SimEnded extends SimStatus {
    }
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("sim-info", SimInfo.class); //$NON-NLS-1$
        xstream.alias("sim-ended", SimEnded.class); //$NON-NLS-1$
        xstream.alias("message", Message.class);        //$NON-NLS-1$
        xstream.alias("prim", SimPrim.class); //$NON-NLS-1$
        xstream.alias("avatar", SimAvatar.class); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    public static SimStatus fromXML(String xml) {
        return (SimStatus) xstream.fromXML(xml);
    }
}
