/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package lslplus;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;

import lslplus.decorators.ErrorDecorator;
import lslplus.editor.LslPartitionScanner;
import lslplus.editor.lsl.LslCodeScanner;
import lslplus.language_metadata.LslConstant;
import lslplus.language_metadata.LslFunction;
import lslplus.language_metadata.LslHandler;
import lslplus.language_metadata.LslMetaData;
import lslplus.language_metadata.LslParam;
import lslplus.lsltest.TestManager;
import lslplus.util.LslColorProvider;
import lslplus.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * The class representing the LSL Plus plugin.
 * @author rgreayer
 *
 */
public class LslPlusPlugin extends AbstractUIPlugin {

    private static class ValidationResult {
        public boolean ok;
        public String msg;
    }
    
    public static final boolean DEBUG = true;

    private static LslPlusPlugin instance;

    public final static String LSL_PARTITIONING = "__lsl_partitioning"; //$NON-NLS-1$
    
    public static final String PLUGIN_ID = "lslplus"; //$NON-NLS-1$
    public static Image createImage(String path) {
        ImageDescriptor descriptor = imageDescriptorFromPlugin(path);
        if (descriptor != null) return descriptor.createImage();
        return null;
    }
    /**
     * Returns the default plug-in instance.
     * 
     * @return the default plug-in instance
     */
    public static LslPlusPlugin getDefault() {
        return instance;
    }
    public static ImageDescriptor imageDescriptorFromPlugin(String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }
    public static void openResource(Shell shell, final IFile resource) {
        final IWorkbenchPage activePage= 
            (PlatformUI.getWorkbench() != null &&
            PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null) ?
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage() : null;
            
        if (activePage != null) {
            final Display display= shell.getDisplay();
            if (display != null) {
                display.asyncExec(new Runnable() {
                    public void run() {
                        try {
                            IDE.openEditor(activePage, resource, true);
                        } catch (PartInitException e) {
                            Util.log(e, e.getLocalizedMessage());
                        }
                    }
                });
            }
        }
    }
    public static Process runExecutable(String executablePath, String input, boolean redir) {
        URL url = FileLocator.find(getDefault().getBundle(), new Path("$os$/" + executablePath), null); //$NON-NLS-1$

        if (url == null) {
            Util.error("no such executable: " + executablePath); //$NON-NLS-1$
            return null;
        }

        try {
            File f = new File(FileLocator.toFileURL(url).getFile());

            if (f == null) {
                Util.error("couldn't find executable: " + executablePath); //$NON-NLS-1$
                return null;
            }

            ProcessBuilder builder = new ProcessBuilder(new String[] { f.getPath() });
            builder.redirectErrorStream(redir);
            Process process = builder.start();

            OutputStream out = process.getOutputStream();
            OutputStreamWriter writer = new OutputStreamWriter(out);

            writer.write(input);
            writer.close();
            return process;
        } catch (IOException e) {
            Util.log(e, e.getMessage());
            return null;
        }
    }
    
    public static String runTask(String executablePath, String input) {
        Process p = runExecutable(executablePath, input, true);
        
        if (p == null) return null;
        
        StringBuilder buf = new StringBuilder();
        InputStreamReader reader = new InputStreamReader(p.getInputStream());
        
        char[] chars = new char[512];
        int count = 0;
        
        try {
            while ((count = reader.read(chars)) >= 0) {
                buf.append(chars, 0, count);
            }
            
            return buf.toString();
        } catch (IOException e) {
            Util.log(e,e.getLocalizedMessage());
            return null;
        } finally {
            try { 
                reader.close();
            } catch (IOException e) {
                Util.log(e, e.getLocalizedMessage());
            }
        }
    }
    
    public static String validateExpression(String expression) {
        String result = runTask("ExpressionHandler.exe", expression); //$NON-NLS-1$
        XStream xstream = new XStream(new DomDriver());
        xstream.alias("result", ValidationResult.class); //$NON-NLS-1$
        ValidationResult e = (ValidationResult) xstream.fromXML(result);
        
        if (e.ok) return null;
        return e.msg;
    }
    
    private LslCodeScanner fCodeScanner;

    private LslColorProvider fColorProvider;

    private ErrorDecorator fErrorDecorator;

    private LslPartitionScanner fPartitionScanner;

    private LslMetaData lslMetaData = null;

    private TestManager testManager = null;

    /**
     * Creates a new plug-in instance.
     */
    public LslPlusPlugin() {
        instance = this;
        testManager = new TestManager();
    }

    private LslMetaData buildMetaData() {
        String result = runTask("MetaData.exe", ""); //$NON-NLS-1$//$NON-NLS-2$
        if (result == null) {
            Util.error(Messages.LslPlusPlugin_NO_META_DATA);
            return new LslMetaData();
        }
        if (DEBUG) Util.log("Meta-Data: " + result); //$NON-NLS-1$
        XStream xstream = new XStream(new DomDriver());

        xstream.alias("lslmeta", LslMetaData.class); //$NON-NLS-1$
        xstream.alias("handler", LslHandler.class); //$NON-NLS-1$
        xstream.alias("param", LslParam.class); //$NON-NLS-1$
        xstream.alias("function", LslFunction.class); //$NON-NLS-1$
        xstream.alias("constant", LslConstant.class); //$NON-NLS-1$
        LslMetaData md = null;
        try {
            md = (LslMetaData) xstream.fromXML(result);
        } catch (Exception e) {
            Util.log(e, Messages.LslPlusPlugin_COULD_NOT_DESERIALIZE_META_DATA);
            md = new LslMetaData();
        }
        return md;
    }

    public void errorStatusChanged() {
        if (fErrorDecorator != null) {
            fErrorDecorator.errorStatusChanged();
        }
    }

    /**
     * Returns the singleton LSL code scanner.
     * 
     * @return the singleton LSL code scanner
     */
    public RuleBasedScanner getLslCodeScanner() {
        if (fCodeScanner == null) {
            String[] handlerNames = (String[]) Util.arrayMap(new Util.ArrayMapFunc() {
                public Class elementType() {
                    return String.class;
                }

                public Object map(Object o) {
                    return ((LslHandler) o).getName();
                }
            }, getLslMetaData().getHandlers());
            String[] predefFuncs = (String[]) Util.arrayMap(new Util.ArrayMapFunc() {
                public Class elementType() {
                    return String.class;
                }

                public Object map(Object o) {
                    return ((LslFunction) o).getName();
                }
            }, getLslMetaData().getFunctions());
            String[] predefConsts = (String[]) Util.arrayMap(new Util.ArrayMapFunc() {
                public Class elementType() {
                    return String.class;
                }

                public Object map(Object o) {
                    return ((LslConstant) o).getName();
                }
            }, getLslMetaData().getConstants());
            fCodeScanner = new LslCodeScanner(getLslColorProvider(), handlerNames, predefFuncs,
                    predefConsts);
        }
        return fCodeScanner;
    }

    /**
     * Returns the singleton Java color provider.
     * 
     * @return the singleton Java color provider
     */
    public LslColorProvider getLslColorProvider() {
        if (fColorProvider == null)
            fColorProvider = new LslColorProvider();
        return fColorProvider;
    }
    
    public synchronized LslMetaData getLslMetaData() {
        if (lslMetaData == null) {
            lslMetaData = buildMetaData();
        }
        return lslMetaData;
    }
    
    /**
     * Return a scanner for creating LSL Plus partitions.
     * 
     * @return a scanner for creating Java partitions
     */
    public LslPartitionScanner getLslPartitionScanner() {
        if (fPartitionScanner == null)
            fPartitionScanner = new LslPartitionScanner();
        return fPartitionScanner;
    }

    public TestManager getTestManager() {
        return testManager;
    }
    
    public void setErrorDecorator(ErrorDecorator errorDecorator) {
        this.fErrorDecorator = errorDecorator;
    }

}