package lslplus.sim;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import lslplus.gentree.Node;
import lslplus.gentree.NodeFactory;
import lslplus.gentree.NodeStatus;
import lslplus.sim.SimProject.AvatarNode;
import lslplus.sim.SimProject.AvatarReferenceNode;
import lslplus.sim.SimProject.FixedFormatNode;
import lslplus.sim.SimProject.HasDerivedValue;
import lslplus.sim.SimProject.PrimNode;
import lslplus.sim.SimProject.StringNode;
import lslplus.sim.SimWorldDef.InventoryItem;
import lslplus.sim.SimWorldDef.LVector;
import lslplus.sim.SimWorldDef.Prim;
import lslplus.sim.SimWorldDef.Region;
import lslplus.sim.SimWorldDef.Landmark;

public class SimProjectNodes {

    public static class InventoryPropertiesNode extends FixedFormatNode {

        public InventoryPropertiesNode(Node parent, String creator) {
            super(parent, "properties", null); //$NON-NLS-1$
            addChild(new AvatarReferenceNode(this, "creator", creator));
        }

        public String getNameDisplay() { return "Properties"; }
        public Map getData() {
            HashMap map = new HashMap();
            for (Iterator i = this.getChildren().iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                
                String name = n.getName();
                if (n instanceof AvatarReferenceNode) {
                    map.put(name, n.getValueString());
                } else if (n instanceof HasDerivedValue) {
                    map.put(name, ((HasDerivedValue)n).getDerivedValue());
                } else {
                    map.put(name, n.getValue());
                }
            }
            
            return map;
        }
        
    }
    public static abstract class InventoryNode extends Node {
        private static final NodeStatus SCRIPT_NAME_IN_USE = new NodeStatus(false, "Script name already in use");
        
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
        
        protected void addProperty(Node n) {
            InventoryPropertiesNode pn = (InventoryPropertiesNode) findChildByName("properties");
            pn.addChild(n);
        }
        
        public NodeStatus checkNameString(String name) {
            return SimProject.checkNameUnique(this, name, getParent().getChildren(), SCRIPT_NAME_IN_USE);
        }

        protected Map getProperties() {
            InventoryPropertiesNode node = (InventoryPropertiesNode) findChildByName("properties");
            return node.getData();
        }
        
        public abstract InventoryItem getInventoryItem();
        
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

        protected void onUpdate(String s) {
        }
        
        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

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

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            List lineNodes = findChildrenByType(NotecardLineNode.class);
            String[] lines = new String[lineNodes.size()];
            int j = 0;
            for (Iterator i = lineNodes.iterator(); i.hasNext(); ) {
                lines[j++] = ((NotecardLineNode)i.next()).getValueString();  
            }
            return new SimWorldDef.Notecard(getName(), creator, lines);
        }
    }
    
    static final NodeFactory textureFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Texture");
            
            return new TextureNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Texture";
        }
    };
    
    public static class TextureNode extends InventoryNode {

        public TextureNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            return new SimWorldDef.Texture(getName(), creator);
        }
    }
    
    static final NodeFactory bodyPartFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Body Part");
            
            return new BodyPartNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Body Part";
        }
    };

    public static class BodyPartNode extends InventoryNode {

        public BodyPartNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            return new SimWorldDef.BodyPart(getName(), creator);
        }
    }
    
    static final NodeFactory gestureFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Gesture");
            
            return new GestureNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Gesture";
        }
    };

    public static class GestureNode extends InventoryNode {

        public GestureNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            return new SimWorldDef.Gesture(getName(), creator);
        }
    }
    
    static final NodeFactory clothingFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Clothing");
            
            return new ClothingNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Clothing";
        }
    };

    public static class ClothingNode extends InventoryNode {

        public ClothingNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            return new SimWorldDef.Clothing(getName(), creator);
        }
    }

    static final NodeFactory soundFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Sound");
            
            return new SoundNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Sound";
        }
    };

    public static class SoundNode extends InventoryNode {

        public SoundNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.ConstrainedFloatNode(this, "duration", new Float(1.0f), 0.0f, 10f, "Duration"));
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            Float duration = (Float) props.get("duration");
            return new SimWorldDef.Sound(getName(), creator, duration.floatValue());
        }
    }
    
    static final NodeFactory animationFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Animation");
            
            return new AnimationNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Animation";
        }
    };

    public static class AnimationNode extends InventoryNode {

        public AnimationNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.ConstrainedFloatNode(this, "duration", new Float(1.0f), 0.0f, 120f, "Duration"));
        }

        public NodeFactory[] legalChildNodes() { return SimProject.EMPTY_FACTORY_LIST; }
        
        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            String creator = (String) props.get("creator");
            Float duration = (Float) props.get("duration");
            return new SimWorldDef.Sound(getName(), creator, duration.floatValue());
        }
    }
    
    static final NodeFactory landmarkFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Landmark");
            
            return new LandmarkNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Landmark";
        }
    };
    
    public static class LandmarkNode extends InventoryNode {

        public LandmarkNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addProperty(new SimProject.GridPositionNode(this,"position")); //$NON-NLS-1$
            addProperty(new SimProject.RegionNode(this,"region", "Region"));  //$NON-NLS-1$//$NON-NLS-2$
        }

        public InventoryItem getInventoryItem() {
            Map props = getProperties();
            Region region = (Region) props.get("region"); //$NON-NLS-1$
            LVector position = (LVector) props.get("position"); //$NON-NLS-1$
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            return new Landmark(getName(), creator, region, position);
        }

        public NodeFactory[] legalChildNodes() {
            return SimProject.EMPTY_FACTORY_LIST; 
        }
    }
    
    static final NodeFactory inventoryObjectFactory = new NodeFactory() {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "Object");
            
            InventoryObjectNode object = new InventoryObjectNode(parent, name, null);
            
            PrimNode rootPrim = (PrimNode) SimProject.primNodeFactory.createNode(node);
            rootPrim.setName(name);
            object.addChild(rootPrim);
            return object;
        }

        public String getNodeTypeName() {
            return "Inventory Object";
        }
    };
    
    public static class InventoryObjectNode extends InventoryNode {
        private static final NodeFactory[] LEGAL_CHILDREN = { SimProject.primNodeFactory };
        public InventoryObjectNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
        }

        public InventoryItem getInventoryItem() {
            List primNodes = findChildrenByType(PrimNode.class);
            Prim prims[] = new Prim[primNodes.size()];
            int j = 0;
            for (Iterator i = primNodes.iterator(); i.hasNext(); ) {
                prims[j++] = (Prim) ((PrimNode)i.next()).getDerivedValue();
            }
            
            Map props = getProperties();
            String creator = (String) props.get("creator"); //$NON-NLS-1$
            return new SimWorldDef.InventoryObject(getName(),creator,prims);
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }
    }
   
}
