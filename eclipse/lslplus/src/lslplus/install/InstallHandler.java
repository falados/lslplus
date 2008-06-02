package lslplus.install;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.update.core.BaseInstallHandler;
import org.eclipse.update.core.IPluginEntry;
import org.eclipse.update.core.ISite;

public class InstallHandler extends BaseInstallHandler {

    public void completeConfigure() throws CoreException {
        try {
            if (File.separatorChar == '\\') return; // windows...
            ISite site = feature.getSite();
            URL url = FileLocator.toFileURL(site.getURL());
            String path = url.getFile();
            File sitedir = new File(path);
            
            IPluginEntry[] entries = feature.getPluginEntries();
            for (int i = 0; i < feature.getPluginEntries().length; i++) {
                String s = entries[i].getVersionedIdentifier().toString();
                String path1 = sitedir.getPath() + File.separator + "plugins" + File.separator + s;
                
                File pluginDir = new File(path1);
                doit(pluginDir);
                System.out.println(path1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void doit(File f) {
        if (f.isDirectory()) {
            File[] files = f.listFiles();
            for (int i = 0; i < files.length; i++) {
                doit(files[i]);
            }
        } else if ("LslPlus".equals(f.getName())) {
            ProcessBuilder builder = new ProcessBuilder(new String[] {"chmod", "+x", f.getAbsolutePath()});
            try {
                builder.redirectErrorStream(true);
                Process p = builder.start();
                
                InputStreamReader reader = new InputStreamReader(p.getInputStream());
                int c;
                
                while ((c = reader.read()) >= 0) {
                    char cc = (char)c;
                    System.out.print(cc);
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            System.out.println("doit to: " + f.getAbsolutePath());
        }
    }

}
