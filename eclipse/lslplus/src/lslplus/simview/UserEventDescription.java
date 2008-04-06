package lslplus.simview;

public class UserEventDescription {
    public abstract static class ParameterDescription {
        abstract public String getName();
        abstract public String getDescription();
        abstract public String getControlID();
    }
    
    public static class PrimParameter extends ParameterDescription {
        public String getName() { return "Prim"; }
        public String getDescription() { return "Key/Name of the prim to send the event to;"; }
        public String getControlID() { return "prim"; }
    }
    
    public static class ValueParameter extends ParameterDescription {
        private String type;
        private String description;
        private String name;
        
        public ValueParameter(String type, String name, String description) {
            this.type = type;
            this.description = description;
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
        
        public String getDescription() {
            return description;
        }
        
        public String getType() {
            return type;
        }
        
        public String getControlID() {
            return "expression-" + type; //$NON-NLS-1$
        }
    }
    
    private ParameterDescription[] parameters;
    private String eventName;
    private String eventDescription;
    
    public UserEventDescription(String eventName, String eventDescription, 
            ParameterDescription[] parameters) {
        this.eventName = eventName;
        this.eventDescription = eventDescription;
        this.parameters = (ParameterDescription[]) parameters.clone();
    }

    public ParameterDescription[] getParameters() {
        return (ParameterDescription[]) parameters.clone();
    }

    public String getEventName() {
        return eventName;
    }

    public String getEventDescription() {
        return eventDescription;
    }
    
}
