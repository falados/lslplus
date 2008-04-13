package lslplus.sim;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 *
 * Classes used for serializing setup info to haskell core.
 */
public class SimWorldDef {
    public static class Script {
        private String primKey;
        private String scriptName;
        private String scriptId;
        
        public Script(String primKey, String scriptName, String scriptId) {
            this.primKey = primKey;
            this.scriptName = scriptName;
            this.scriptId = scriptId;
        }
    }
    
    public static class SimObject {
        private String[] primKeys;
        
        public SimObject(String[] primKeys) {
            this.primKeys = primKeys;
        }    
    }
    
    public static class Prim {
        private String name;
        private String key;
        private String[] scripts;
        
        public Prim(String name, String key, String[] scripts) {
            this.name = name;
            this.key = key;
            this.scripts = scripts;
        }
    }
    
    public static class Avatar {
        private String key;
        private String name;
        
        public Avatar(String key, String name) {
            this.key = key;
            this.name = name;
        }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    
    public static void configureXStream(XStream xstream) {
        xstream.alias("world-def", SimWorldDef.class); //$NON-NLS-1$
        xstream.alias("avatar", Avatar.class); //$NON-NLS-1$
        xstream.alias("script", Script.class); //$NON-NLS-1$
        xstream.alias("object", SimObject.class); //$NON-NLS-1$
        xstream.alias("prim", Prim.class); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    public static String toXML(SimWorldDef def) {
        return xstream.toXML(def);
    }
    
    private long maxTime;
    private int sliceSize;
    private Script[] scripts;
    private SimObject[] objects;
    private Prim[] prims;
    private Avatar[] avatars;
    
    public SimWorldDef(long maxTime, int sliceSize, Script[] scripts, SimObject[] objects,
            Prim[] prims, Avatar[] avatars) {
        this.maxTime = maxTime;
        this.sliceSize = sliceSize;
        this.scripts = scripts;
        this.objects = objects;
        this.prims = prims;
        this.avatars = avatars;
    }

    public static SimWorldDef mkSimpleWorld(SimKeyManager keyManager, String name) {
        String primKey = keyManager.getNextKey();
        SimWorldDef.Script[] scripts = new SimWorldDef.Script[] {
                new SimWorldDef.Script(primKey, name, name)
        };
        
        SimWorldDef.Prim[] prims = new SimWorldDef.Prim[] {
                new SimWorldDef.Prim("defaultPrim", primKey, new String[] { name })
        };
        
        SimWorldDef.SimObject[] objects = new SimWorldDef.SimObject[] {
                new SimWorldDef.SimObject(new String[] { primKey })
        };
        
        String avKey = keyManager.getNextKey();
        SimWorldDef.Avatar[] avatars = new SimWorldDef.Avatar[] {
                new SimWorldDef.Avatar(avKey, "Default Avatar")
        };
        
        SimWorldDef def = new SimWorldDef(10000000,1000,scripts,objects,prims,avatars);
        
        return def;
    }
    
    
}
