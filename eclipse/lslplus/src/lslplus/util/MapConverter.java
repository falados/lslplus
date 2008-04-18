package lslplus.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.ConversionException;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.ExtendedHierarchicalStreamWriterHelper;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.mapper.Mapper;

public class MapConverter implements Converter {
    private Mapper mapper;
    
    public MapConverter(Mapper mapper) {
        this.mapper = mapper;
    }
    public void marshal(Object source, HierarchicalStreamWriter writer, MarshallingContext context) {
        Map map = (Map) source;
        
        for (Iterator iterator = map.entrySet().iterator(); iterator.hasNext();) {
            Map.Entry entry = (Map.Entry) iterator.next();

            String key = entry.getKey().toString();
            Object value = entry.getValue();
            writer.startNode("entry");
            writer.startNode("key");
            writer.setValue(key);
            writer.endNode();
            
            ExtendedHierarchicalStreamWriterHelper.startNode(writer, "value", value.getClass()); //$NON-NLS-1$
            if (!value.getClass().equals(Object.class)) {
                writer.addAttribute(mapper.aliasForAttribute("class"), mapper.serializedClass(value.getClass()));
            }

            context.convertAnother(value);
            writer.endNode();

            writer.endNode();
        }
        
    }

    public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
        Map m = (Map) createCollection(context.getRequiredType());
        while (reader.hasMoreChildren()) {
            reader.moveDown();
            String key = null;
            Object value = null;
            while (reader.hasMoreChildren()) {
                reader.moveDown();
                
                if ("key".equals(reader.getNodeName())) {
                    if (key != null) throw new ConversionException("multiple keys in single entry");
                    key = context.convertAnother(m, String.class).toString();
                    
               } else if ("value".equals(reader.getNodeName())) {
                    if (value != null) throw new ConversionException("multiple values in single entry");
                    value = readItem(reader, context, m);
                } else {
                    throw new ConversionException("unrecognized InfoMap element");
                }
                
                reader.moveUp();
            }
            reader.moveUp();
            
            m.put(key, value);
        }
        return m;
    }

    public boolean canConvert(Class type) {
        return Map.class.isAssignableFrom(type);
    }

    private Object createCollection(Class type) {
        Class defaultType = mapper.defaultImplementationOf(type);
        try {
            return defaultType.newInstance();
        } catch (InstantiationException e) {
            throw new ConversionException("Cannot instantiate " + defaultType.getName(), e);
        } catch (IllegalAccessException e) {
            throw new ConversionException("Cannot instantiate " + defaultType.getName(), e);
        }
    }
    
    private Object readItem(HierarchicalStreamReader reader, UnmarshallingContext context, Object current) {
        String classAttribute = reader.getAttribute(mapper.aliasForAttribute("class"));
        Class type;
        if (classAttribute == null) {
            type = mapper.realClass(reader.getNodeName());
        } else {
            type = mapper.realClass(classAttribute);
        }
        return context.convertAnother(current, type);

    }
    
    public static void main(String[] args) {
        XStream xstream = new XStream(new DomDriver());
        Mapper mapper = xstream.getMapper();
        
        xstream.alias("valueDB", Map.class);
        xstream.registerConverter(new MapConverter(mapper), XStream.PRIORITY_VERY_HIGH);
        xstream.alias("list", List.class);
        xstream.registerConverter(new ListConverter(mapper));
        HashMap m = new HashMap();
        m.put("hello", "there");
        m.put("goodbye", new Integer(12));
        HashMap m1 = new HashMap();
        m1.put("foo", new Float(5.0));
        ArrayList l = new ArrayList();
        l.add("bite");
        l.add(new Integer(90210));
        m1.put("list", l);
        m.put("more", m1);
        String s = xstream.toXML(m);
        System.out.println(xstream.toXML(m));
        Object o = xstream.fromXML(s);
        String s1 = xstream.toXML(o);
        System.out.println(s1);
        
        System.out.println(s1.equals(s));
        
        
        System.out.println(xstream.toXML(l));
    }
}
