package lslplus.sim;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lslplus.sim.SimProject.AvatarNode;
import lslplus.sim.SimProject.AvatarNodeProxy;
import lslplus.sim.SimProject.AvatarReferenceNode;
import lslplus.sim.SimProject.FixedFormatNode;
import lslplus.sim.SimProject.Node;
import lslplus.sim.SimProject.NodeFactory;
import lslplus.sim.SimProject.PrimNode;
import lslplus.sim.SimProject.Status;
import lslplus.sim.SimProject.StringNode;
import lslplus.sim.SimWorldDef.InventoryItem;

public class SimProjectNodes {

    public static class InventoryPropertiesNode extends FixedFormatNode {

        public InventoryPropertiesNode(Node parent, String creator) {
            super(parent, "properties", null); //$NON-NLS-1$
            addChild(new AvatarReferenceNode(this, "creator", creator));
        }

        public Map getData() {
            HashMap map = new HashMap();
            AvatarReferenceNode avNode = (AvatarReferenceNode) findChildByName("creator");
            map.put("creator", new AvatarNodeProxy(avNode.getValueString()));
            return map;
        }
        
    }
    public static abstract class InventoryNode extends Node {
        private static final Status SCRIPT_NAME_IN_USE = new Status(false, "Script name already in use");

        public InventoryNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            String owner = null;
            if (parent instanceof AvatarNode) {
                owner = ((AvatarNode)parent).getName();
            } else {
                PrimNode node = (PrimNode) parent;
                owner = node.getOwner();
            }
            addChild(new InventoryPropertiesNode(this, owner));
        }
        
        public Status checkNameString(String name) {
            return SimProject.checkNameUnique(this, name, getParent().getChildren(), SCRIPT_NAME_IN_USE);
        }

        protected Map getProperties() {
            InventoryPropertiesNode node = (InventoryPropertiesNode) findChildByName("properties");
            return node.getData();
        }
        
        public abstract InventoryItem getInventoryItem();
    }
    
    public static class NotecardLineNode extends StringNode {

        public NotecardLineNode(Node parent, String value) {
            super(parent, "line", value);
        }

        public boolean isDeletable() {
            return true;
        }
    }
 
    static final NodeFactory notecardLineFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            return new NotecardLineNode(parent,"");
        }

        public String getNodeTypeName() {
            return "Line";
        }
        
    };

    static final NodeFactory notecardFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Notecard");
            
            return new NotecardNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Notecard";
        }
    };
    
    public static class NotecardNode extends InventoryNode {
        private static final NodeFactory[] LEGAL_CHILD_NODES = { notecardLineFactory };
        public NotecardNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public Status checkValueString(String s) {
            return SimProject.OK;
        }

        public String getValueString() {
            return "";
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        protected void onUpdate(String s) {
        }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            AvatarNodeProxy av = (AvatarNodeProxy) props.get("creator");
            List lines = findChildrenByType(NotecardLineNode.class);
            return null;//new SimWorldDef.Notecard(getName(), av.)
        }
    }
}
