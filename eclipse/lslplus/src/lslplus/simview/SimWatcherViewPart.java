package lslplus.simview;

import lslplus.LslPlusPlugin;
import lslplus.SimListener;
import lslplus.SimManager;
import lslplus.sim.SimEvent;
import lslplus.sim.SimEventDefinition;
import lslplus.sim.SimStatuses;
import lslplus.util.Util;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

public class SimWatcherViewPart extends ViewPart implements SimListener {
    public static final String ID = "lslplus.simWatcher"; //$NON-NLS-1$
    private class StopAction extends Action {
        public StopAction(Shell parentShell) {
            setText("Stop Sim"); //$NON-NLS-1$
            setToolTipText("Stop Sim"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/stop.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            getSimManager().stopSim();
        }
    }

    private class TouchAction extends Action {
        private Shell parentShell;
        public TouchAction(Shell parentShell) {
            this.parentShell = parentShell;
            setText("Touch"); //$NON-NLS-1$
            setToolTipText("Touch!"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/touch.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            SimEventDefinition def = simManager.getAnEventDefinition("Touch Prim");
            
            if (def == null) {
                Util.error("event definition not found: Touch Prim");
                return;
            }
            EventDialog dlg = //new TouchDialog(parentShell);
                    new EventDialog(parentShell, def);
            dlg.open();
            
//            if (dlg.getReturnCode() == Window.OK) {
//                SimEvent event = new SimEvent("Touch Prim", 0,
//                        new SimEventArg[] {
//                            new SimEventArg("Prim", "20000000-0000-0000-0000-000000000000"),
//                            new SimEventArg("Avatar", "10000000-0000-0000-0000-000000000000"),
//                            new SimEventArg("Duration", "1.0")
//                        });
            SimEvent event = dlg.getEvent();
            if (event != null) {
                simManager.putEvent(event);
            }
        }
    }

    private Composite parent;
    private Composite counterComposite;
    private SashForm sashForm;
    private TreeViewer logViewer;
    private SimManager simManager;

    private LogViewerContentProvider logViewerModel;
    private TreeColumn fColumn1;
    private TreeColumn fColumn2;


    private Tree logViewerTree;
    private TouchAction touchAction;
    private StopAction stopAction;
    private TreeColumn fColumn3;
    private LogViewerLabelProvider labelProvider;

    public SimWatcherViewPart() {
        simManager = LslPlusPlugin.getDefault().getSimManager();
        simManager.addSimListener(this);
    }

    public void dispose() {
        super.dispose();
        simManager.removeSimListener(this);
        if (labelProvider != null) labelProvider.dispose();
    }

    public boolean isCreated() {
        return counterComposite != null;
    }

    public void createPartControl(Composite parent) {
        this.parent = parent;
        GridLayout gridLayout = new GridLayout();
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        parent.setLayout(gridLayout);
        SashForm sashForm = createSashForm(parent);
        sashForm.setLayoutData(new GridData(GridData.FILL_BOTH));
        configureToolBar();
    }

    private SashForm createSashForm(Composite parent2) {
        sashForm = new SashForm(parent, SWT.VERTICAL);

        ViewForm top = new ViewForm(sashForm, SWT.NONE);

        Composite empty = new Composite(top, SWT.NONE);
        empty.setLayout(new Layout() {
            protected Point computeSize(Composite composite, int wHint, int hHint,
                    boolean flushCache) {
                return new Point(1, 1);
            }

            protected void layout(Composite composite, boolean flushCache) {
            }
        });
        top.setTopLeft(empty);
        logViewer = createLogViewer(top);
        top.setContent(logViewer.getControl());

        return sashForm;
    }

    private TreeViewer createLogViewer(Composite parent) {
        logViewer = new TreeViewer(parent, SWT.SINGLE);
        logViewerTree = logViewer.getTree();
        createColumns(logViewerTree);
        logViewerModel = new LogViewerContentProvider();
        logViewer.setContentProvider(logViewerModel);
        labelProvider = new LogViewerLabelProvider();
        logViewer.setLabelProvider(labelProvider);
        logViewer.setInput(this);
        return logViewer;
    }
    
    private void createColumns(Tree tree) {
        fColumn1 = new TreeColumn(tree, SWT.LEFT);
        fColumn1.setText("Time");  //$NON-NLS-1$
        fColumn1.setWidth(50);
        fColumn1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        fColumn2 = new TreeColumn(tree, SWT.LEFT);
        fColumn2.setText("Source");  //$NON-NLS-1$
        fColumn2.setWidth(200);
        fColumn2.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
        
        fColumn3 = new TreeColumn(tree, SWT.LEFT);
        fColumn3.setText("Message");  //$NON-NLS-1$
        fColumn3.setWidth(300);
        fColumn3.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        tree.setHeaderVisible(true);
    }

    public void setFocus() {
    }

    private void configureToolBar() {
        IActionBars actionBars = getViewSite().getActionBars();
        IToolBarManager toolBar = actionBars.getToolBarManager();
        
        touchAction = new TouchAction(this.getSite().getShell());
        stopAction = new StopAction(this.getSite().getShell());
        touchAction.setEnabled(simManager.isSimActive());
        stopAction.setEnabled(simManager.isSimActive());
        toolBar.add(touchAction);
        toolBar.add(stopAction);

        toolBar.add(new Separator());

        actionBars.updateActionBars();
    }

    public void newLogMessages(SimStatuses.Message[] result) {
        this.logViewerModel.addMessages(result);
        refreshAsync();
    }

    public void simLaunched() {
        touchAction.setEnabled(true);
        stopAction.setEnabled(true);
        if (logViewerModel != null) {
            this.logViewerModel.clear();
            refreshAsync();
        }
    }

    private Runnable refresher = new Runnable() {
        public void run() {
            logViewer.refresh();
        }
    };

    private void refreshAsync() {
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(refresher);
    }
    
    private SimManager getSimManager() {
        return simManager;
    }

    public void simEnded() {
        touchAction.setEnabled(false);
        stopAction.setEnabled(false);
    }
}
