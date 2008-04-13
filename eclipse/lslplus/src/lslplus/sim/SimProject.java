package lslplus.sim;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import lslplus.sim.SimWorldDef.Avatar;
import lslplus.sim.SimWorldDef.Prim;
import lslplus.sim.SimWorldDef.Script;
import lslplus.sim.SimWorldDef.SimObject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

// TODO: add validation, so we can mark project is invalid, e.g. if referenced scripts don't exist.
public class SimProject {
    private static final Status OK = new Status(true,"");
    public static class Status {
        private boolean ok;
        private String error;
    
        public Status(boolean ok, String error) {
            this.ok = ok;
            this.error = error;
        }
        
        public String toString() { return error; }
        
        public boolean isOk() { return ok; }
    }
    
    public static interface NodeVisitor {
        public void visit(Node n);
    }
    
    public static interface NodeListener {
        public void nodeValueChanged(Node n);
        public void nodeStructureChanged(Node n);
    }
    
    public static abstract class Node {
        private String nodeName;
        private Object value;
        private Node parent;
        private ArrayList children;
        private Node[] childrenArray;
        private HashSet listeners = new HashSet();
        
        public Node(Node parent, String nodeName, Object value) {
            this.parent = parent;
            this.nodeName = nodeName;
            this.value = value;
        }
        
        public void addListener(NodeListener l) {
            if (listeners == null) listeners = new HashSet();
            listeners.add(l);
            
            for (Iterator i = getChildren().iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                n.addListener(l);
            }
        }
        
        public void removeListener(NodeListener l) {
            if (listeners == null) return;
            listeners.remove(l);
            for (Iterator i = getChildren().iterator(); i.hasNext(); ) {
                Node n = (Node) i.next();
                n.removeListener(l);
            }
        }
        
        public String getNodeName() {
            return nodeName;
        }
        
        public void setNodeName(String nodeName) {
            this.nodeName = nodeName;
            fireValueChanged();
        }
        
        public Object getValue() {
            return value;
        }
        
        public void setValue(Object value) {
            this.value = value;
            fireValueChanged();
        }
        
        private void fireValueChanged() {
            for (Iterator i = listeners.iterator(); i.hasNext();) {
                NodeListener listener = (NodeListener) i.next();
                listener.nodeValueChanged(this);
            }
        }

        public Node getParent() {
            return parent;
        }
        
        public void setParent(Node parent) {
            this.parent = parent;
        }
        
        public ArrayList getChildren() {
            if (children == null) {
                if (childrenArray == null) childrenArray = new Node[0];
                children = new ArrayList();
                Collections.addAll(children, childrenArray);
            }
            return children;
        }
        
        public void addChild(Node n) {
            getChildren().add(n);
            
            propagateListeners(n);
            fireStructureChanged();
        }

        private void propagateListeners(Node n) {
            for (Iterator i = listeners.iterator(); i.hasNext();) {
                NodeListener l = (NodeListener) i.next();
                n.addListener(l);
            }
        }

        public void insertChildBefore(Node newChild, Node existingChild) {
            int index = getChildren().indexOf(existingChild);
            if (index < 0) getChildren().add(0, newChild);
            else getChildren().add(index, newChild);
            
            propagateListeners(newChild);
            fireStructureChanged();
        }
        
        public void insertChildAfter(Node newChild, Node existingChild) {
            int index = getChildren().indexOf(existingChild);
            if (index < 0) getChildren().add(newChild);
            else getChildren().add(index + 1, newChild);
            
            propagateListeners(newChild);
            fireStructureChanged();
        }
        
        
        public void removeChild(Node n) {
            if (getChildren().remove(n)) fireStructureChanged();
        }
        
        private void fireStructureChanged() {
            for (Iterator i = listeners.iterator(); i.hasNext();) {
                NodeListener listener = (NodeListener) i.next();
                listener.nodeStructureChanged(this);
            }
        }

        public abstract NodeFactory[] legalChildNodes();
        
        public abstract String getValueString();
        
        public abstract Status checkValueString(String s);
        
        public abstract void setValueFromString(String s);
        
        public abstract boolean isValueChangeable();
        
        public abstract boolean isNameChangeable();
        
        public abstract Status checkNameString(String name);
        
        public abstract boolean isDeletable();
    
        public void accept(NodeVisitor visitor) {
            visitor.visit(this);
            for (Iterator i = getChildren().iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                n.accept(visitor);
            }
        }
        
        public Node findChildByName(String name) {
            for (Iterator i = children.iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                if (n.getNodeName().equals(name)) {
                    return n;
                }
            }
            
            return null;
        }
        
        public List findChildrenByType(Class c) {
            ArrayList list = new ArrayList();
            for (Iterator i = children.iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                if (c.equals(n.getClass())) {
                    list.add(n);
                }
            }
            
            return list;
        }
        
        public boolean hasValueChoices() {
            return false;
        }
        
        public String getChoicesId() {
            return null;
        }
        
        public void propagateParent() {
            for (Iterator i = getChildren().iterator(); i.hasNext();) {
                Node n = (Node) i.next();
                n.setParent(this);
                n.propagateParent();
            }
        }
        
        public void syncChildren() {
            List children = getChildren();
            childrenArray = (Node[]) children.toArray(new Node[children.size()]);
            
            for (int i = 0; i < childrenArray.length; i++) {
                childrenArray[i].syncChildren();
            }
        }
    }

    public static interface NodeFactory {
        public String getNodeTypeName();
        public Node createNode(Node parent);
    }

    private static ObjectNodeFactory objectNodeFactory = new ObjectNodeFactory();
    private static PrimNodeFactory primNodeFactory = new PrimNodeFactory();
    private static ScriptNodeFactory scriptNodeFactory = new ScriptNodeFactory();
    private static AvatarNodeFactory avatarNodeFactory = new AvatarNodeFactory();

    public static class ObjectNodeFactory implements NodeFactory {
        public Node createNode(Node parent) {
            ObjectNode node = new ObjectNode(parent, "Object");
            PrimNode rootPrim = (PrimNode) primNodeFactory.createNode(node);
            rootPrim.setNodeNameWithoutChangingParentName("Object");
            node.addChild(rootPrim);
            
            return node;
        }

        public String getNodeTypeName() {
            return "Object";
        }
    }

    public static class PrimNodeFactory implements NodeFactory {
        public Node createNode(Node parent) {
            String name = computeNewName(parent.getChildren(), "prim");
            return new PrimNode(parent, name);
        }

        public String getNodeTypeName() {
            return "Prim";
        }
    }

    public static class ScriptNodeFactory implements NodeFactory {

        public Node createNode(Node parent) {
            PrimNode node = (PrimNode) parent;
            List nodes = node.getChildren();
            String name = SimProject.computeNewName(nodes, "New Script");
            
            return new ScriptNode(parent, name, "");
        }

        public String getNodeTypeName() {
            return "Script";
        }
    }
    
    public static class AvatarNodeFactory implements NodeFactory {
        public Node createNode(Node parent) {
            WorldNode world = (WorldNode) parent;
            String name = computeNewName(world.getChildren(), "Joe Avatar");
            
            return new AvatarNode(parent, name, null);
        }

        public String getNodeTypeName() {
            return "Avatar";
        }
    }

    public static class WorldNode extends Node implements IAdaptable {
        private static final NodeFactory[] LEGAL_CHILD_NODES = { objectNodeFactory, avatarNodeFactory };
        private IResource resource;
        public WorldNode(String name) {
            super(null, name,"");
            addChild(new AnyNaturalNode(this, "max_time", 10000000));
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public String getValueString() {
            return getValue().toString();
        }

        public Status checkValueString(String s) {
            return OK;
        }

        public void setValueFromString(String s) {
            this.setValue(s);
        }

        public boolean isValueChangeable() {
            return false;
        }

        public Status checkNameString(String name) {
            return OK;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isDeletable() {
            return false;
        }
        
        public void setResource(IResource r) {
            resource = r;
        }
        
        public IResource getResource() { return resource; }

        public Object getAdapter(Class adapter) {
            return Platform.getAdapterManager().getAdapter(this, adapter);
        }
    }
    
    public static class ObjectNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = { primNodeFactory };
        public ObjectNode(Node parent, String name) {
            super(parent, name, null);
            setParent(parent);
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public String getValueString() {
            return "";
        }

        public Status checkValueString(String s) {
            return OK;
        }

        public void setValueFromString(String s) {
        }

        public boolean isValueChangeable() {
            return false;
        }

        public Status checkNameString(String name) {
            return OK;
        }

        public void setNodeName(String name) {
            List prims = findChildrenByType(PrimNode.class);
            Node root = (Node) prims.get(0);
            super.setNodeName(name);
            root.nodeName = name;
        }
        
        public void setNodeNameWithoutChangingRootPrimName(String name) {
            super.setNodeName(name);
        }
        
        public boolean isNameChangeable() {
            return true;
        }

        public boolean isDeletable() {
            return true;
        }
    }
    
    public static class PrimNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = { scriptNodeFactory };
        
        public PrimNode(Node parent, String name) {
            super(parent, name, null);
            addChild(new PositionNode(this, "X Position", 128));
            addChild(new PositionNode(this, "Y Position", 128));
            addChild(new PositionNode(this, "Z Position", 0));
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public String getValueString() {
            return "";
        }

        public boolean isValueChangeable() {
            return false;
        }

        public Status checkValueString(String s) {
            return OK;
        }

        public void setValueFromString(String s) {
        }

        public Status checkNameString(String name) {
            return OK;
        }

        public void setNodeName(String name) {
            List prims = getParent().findChildrenByType(PrimNode.class);
            if (prims.indexOf(this) == 0) {
                ObjectNode node = (ObjectNode) getParent();
                node.setNodeNameWithoutChangingRootPrimName(name);
            }
            super.setNodeName(name);
        }
        
        public void setNodeNameWithoutChangingParentName(String name) {
            super.setNodeName(name);
        }
        
        public boolean isNameChangeable() {
            return true;
        }

        public boolean isDeletable() {
            List prims = getParent().findChildrenByType(PrimNode.class);
           
            return prims.size() > 1;
        }
    }
    
    public static class AvatarNode extends Node {
        private static final Status AVATAR_NAME_IN_USE = new Status(false, "Avatar name already in use");
        public AvatarNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            addChild(new PositionNode(this, "X Position", 128));
            addChild(new PositionNode(this, "Y Position", 128));
            addChild(new PositionNode(this, "Z Position", 0));
         }

        public NodeFactory[] legalChildNodes() {
            return new NodeFactory[0];
        }

        public String getValueString() {
            return "";
        }

        public boolean isValueChangeable() {
            return false;
        }

        public Status checkValueString(String s) {
            return OK;
        }

        public void setValueFromString(String s) {
        }

        public Status checkNameString(String name) {
            List list = getParent().findChildrenByType(AvatarNode.class);
            Status error = AVATAR_NAME_IN_USE;
            return SimProject.checkNameUnique(this, name, list, error);
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isDeletable() {
            return true;
        }
        
    }
    public static class ScriptNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = new NodeFactory[0];
        private static final Status SCRIPT_NAME_IN_USE = new Status(false, "Script name already in use");
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }
        
        public ScriptNode(Node parent, String name, String scriptId) {
            super(parent, name, scriptId);
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isValueChangeable() {
            return true;
        }

        public Status checkValueString(String s) {
            return OK;
        }

        public void setValueFromString(String s) {
            setValue(s);
        }
        
        public boolean hasValueChoices() {
            return true;
        }
        
        public String getChoicesId() {
            return "scripts";
        }

        public Status checkNameString(String name) {
            return checkNameUnique(this, name, getParent().getChildren(), SCRIPT_NAME_IN_USE);
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isDeletable() {
            return true;
        }
    }
    
    public static class StringNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = new NodeFactory[0];
        private static final Status CANNOT_BE_NULL = new Status(false, "string cannot be null");
        public StringNode(Node parent, String name, String value) {
            super(parent, name, value);
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public String getValueString() {
            
            return (String)getValue();
        }

        public boolean isValueChangeable() {
            return true;
        }

        public Status checkValueString(String s) {
            if (s == null) return new Status(false, "string cannot be null");
            return OK;
        }

        public void setValueFromString(String s) {
            setValue(s);
        }

        public Status checkNameString(String name) {
            return OK;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isDeletable() {
            return false;
        }
    }
    
    public static class PositionNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = new NodeFactory[0];
        private static final Status OUT_OF_RANGE = new Status(false, "number is out of range (0-256");
        public PositionNode(Node parent, String name, float value) {
            super(parent, name, new Float(value));
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public String getValueString() {
            return getValue().toString();
        }

        public boolean isValueChangeable() {
            return true;
        }

        public Status checkValueString(String s) {
            try {
                float f = Float.parseFloat(s);
                
                if (f < 0 || f > 256) return OUT_OF_RANGE;
                return OK;
            } catch (NumberFormatException e) {
                return SimProject.BAD_FORMAT;
            }
        }

        public void setValueFromString(String s) {
            try {
                float f = Float.parseFloat(s);
                setValue(new Float(f));
            } catch (NumberFormatException e) {
                // ignore
            }
        }

        public Status checkNameString(String name) {
            return OK;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isDeletable() {
            return false;
        }
        
    }

    public static class AnyNaturalNode extends Node {
        private static final NodeFactory[] LEGAL_CHILD_NODES = new NodeFactory[0];
        private static final Status OUT_OF_RANGE = new Status(false, "Value is out of range (must be greater than 0)");
        public AnyNaturalNode(Node parent, String name, int value) {
            super(parent, name, new Integer(value));
        }
        
        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILD_NODES;
        }

        public Status checkNameString(String name) {
             return OK;
        }

        public Status checkValueString(String s) {
            try {
                int i = Integer.parseInt(s);
                
                if (i < 0) return OUT_OF_RANGE;
                return OK;
            } catch (NumberFormatException e) {
                return SimProject.BAD_FORMAT;
            }
        }

        public String getValueString() {
            return getValue().toString();
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public void setValueFromString(String s) {
            try {
                int i = Integer.parseInt(s);
                setValue(new Integer(i));
            } catch (NumberFormatException e) {
                // ignore
            }
        }

        public boolean isDeletable() {
            return false;
        }
        
    }

    protected static String computeNewName(List nodes, String prefix) {
        int index = 0;
        
        for (Iterator i = nodes.iterator(); i.hasNext();) {
            Node n = (Node) i.next();
            
            if (n.getNodeName().startsWith(prefix)) {
                String tail = n.getNodeName().substring(prefix.length());
                if (tail.trim().length() > 0) {
                    try {
                        int j = Integer.parseInt(tail);
                        if (j >= index) index = j + 1;
                    } catch (NumberFormatException e) { }
                }
            }
        }
        
        String name = (index > 0) ? prefix + " " + index : prefix;
        return name;
    }

    static Status checkNameUnique(Node n, String name, List list, Status error) {
        for (Iterator i = list.iterator(); i.hasNext();) {
            Node node = (Node) i.next();
            if (node == n) continue;
            if (node.getNodeName() != null && node.getNodeName().equals(name)) return error;
        }
        return OK;
    }

    static final Status BAD_FORMAT = new Status(false, "format of number is incorrect");
    private static XStream xstream = new XStream(new DomDriver());
    
    private static void configureXStream(XStream xstream) {
        Class[] nodeTypes = new Class[] {
                WorldNode.class, AvatarNode.class, ObjectNode.class,
                PrimNode.class, ScriptNode.class, PositionNode.class,
                AnyNaturalNode.class, StringNode.class
        };
        
        xstream.omitField(Node.class, "parent"); //$NON-NLS-1$
        xstream.omitField(Node.class, "children"); //$NON-NLS-1$
        xstream.omitField(Node.class, "listeners"); //$NON-NLS-1$
        for (int i = 0; i < nodeTypes.length; i++) {
            Class c = nodeTypes[i];
            String name = c.getSimpleName();
            name = name.substring(0, 1).toLowerCase() + name.substring(1);
            xstream.alias(name, c);
        }
        
        xstream.omitField(WorldNode.class, "resource"); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    public static String toXml(WorldNode world) {
        world.syncChildren();
        return xstream.toXML(world);
    }

    public static WorldNode fromXml(InputStream contents, IFile file) {
        WorldNode n = (WorldNode) xstream.fromXML(contents);
        n.setResource(file);
        n.propagateParent();
        return n;
    }
    
    private static Object get(HashMap m, Object key, Object defaultVal) {
        Object v = m.get(key);
        if (v == null) v = defaultVal;
        return v;
    }
    
    public static SimWorldDef toSimWorldDef(WorldNode node) {
        final SimKeyManager keyManager = new SimKeyManager();
        final HashMap worldProperties = new HashMap();
        final HashMap objects = new HashMap();
        final HashMap prims = new HashMap();
        final HashSet scripts = new HashSet();
        final HashMap avatars = new HashMap();
        final HashMap reverseKeyMap = new HashMap();
        node.accept(new NodeVisitor() {
            public void visit(Node n) {
                if (n instanceof ObjectNode) {
                    LinkedList l = new LinkedList();
                    objects.put(n, l);
                } else if (n instanceof PrimNode) {
                    LinkedList l = (LinkedList) objects.get(n.getParent());
                    l.add(n);
                    HashMap info = new HashMap();
                    prims.put(n, info);
                    reverseKeyMap.put(n, keyManager.getNextKey());
                    info.put("scripts", new LinkedList());
                    info.put("name", n.getNodeName());
                } else if (n instanceof ScriptNode) {
                    scripts.add(n);
                    Map primInfo = (Map) prims.get(n.getParent());
                    List primScripts = (List) primInfo.get("scripts");
                    primScripts.add(n.getNodeName());
                } else if (n instanceof AvatarNode) {
                    HashMap info = new HashMap();
                    avatars.put(n, info);
                    info.put("name", n.getNodeName());
                    String key = keyManager.getNextKey();
                    reverseKeyMap.put(n, key);
                } else if (n instanceof PositionNode) {
                    HashMap info = (HashMap) ((n.getParent() instanceof AvatarNode) ?
                            avatars.get(n.getParent()) : prims.get(n.getParent()));
                    info.put(n.getNodeName(), n.getValue());
                } else if (n instanceof AnyNaturalNode) {
                    Node parent = n.getParent();
                    if (parent instanceof WorldNode) {
                        // TODO: fix this!
                        worldProperties.put("max_tick", n.getValue());
                    }
                }
            }
            
        });

        Integer max_tick = (Integer) get(worldProperties, "max_tick", new Integer(1000000));
        
        Avatar[] avatarArray = new Avatar[avatars.size()];
        
        int index = 0;
        for (Iterator i = avatars.entrySet().iterator(); i.hasNext();) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) reverseKeyMap.get(entry.getKey());
            HashMap info = (HashMap) entry.getValue();
            String name = (String) info.get("name");
            
            avatarArray[index++] = new Avatar(key, name);
        }
        
        SimObject[] simObjects = new SimObject[objects.size()];
        
        index = 0;
        for (Iterator i = objects.entrySet().iterator(); i.hasNext();) {
            Map.Entry entry = (Map.Entry) i.next();
            List l = (List) entry.getValue();
            String[] primKeys = new String[l.size()];
            int j = 0;
            for (Iterator i1 = l.iterator(); i1.hasNext();) {
                primKeys[j++] = (String) reverseKeyMap.get(i1.next());
            }
            
            simObjects[index++] = new SimObject(primKeys);
        }
        
        Prim[] primArray = new Prim[prims.size()];
        index = 0;
        for (Iterator i = prims.entrySet().iterator(); i.hasNext();) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) reverseKeyMap.get(entry.getKey());
            Map info = (Map) entry.getValue();
            List scriptNames = (List) info.get("scripts");
            
            primArray[index++] = new Prim((String)info.get("name"), key, 
                    (String[])scriptNames.toArray(new String[scriptNames.size()]));
        }
        
        Script[] scriptArray = new Script[scripts.size()];
        index = 0;
        for (Iterator i = scripts.iterator(); i.hasNext(); ) {
            ScriptNode scriptNode = (ScriptNode) i.next();
            String primKey = (String) reverseKeyMap.get(scriptNode.getParent());
            scriptArray[index++] = new Script(primKey, scriptNode.getNodeName(), scriptNode.getValueString());
        }
        return new SimWorldDef(max_tick.intValue(),1000, scriptArray, simObjects, primArray, avatarArray);
    }
    
    public static void main(String[] args) {
        try {
            Class c = SimProject.class.getClassLoader().loadClass("lslplus.sim.SimProject.WorldNode");
            System.out.println("class loaded");
        } catch (ClassNotFoundException e) {
            System.out.println("class not found!");
        }
        WorldNode n = new WorldNode("world"); //$NON-NLS-1$
        n.syncChildren();
//        System.out.println(xstream.toXML(n));
        System.out.println(n.getClass().getName());
    }
}
