package lslplus.simview;

import lslplus.simview.UserEventDescription.ParameterDescription;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

public class EventDialog extends Dialog {
    private Combo combo;
    private UserEventDescription desc;
    protected EventDialog(Shell parentShell, UserEventDescription desc) {
        super(parentShell);
        this.desc = desc;
    }

    private Button okButton() {
        return getButton(IDialogConstants.OK_ID);
    }
    
    private boolean selectionIsValid() {
        return combo.getSelectionIndex() >= 0;
    }

    protected Control createButtonBar(Composite parent) {
        Control c = super.createButtonBar(parent);
        okButton().setEnabled(false);
        okButton().addListener(SWT.Show, new Listener() {
           public void handleEvent(Event event) {
               okButton().setEnabled(selectionIsValid());
           }
        });
        return c;
    }
    
    protected Control createDialogArea(Composite parent) {
        getShell().setText("Event: " + desc.getEventName());
        Composite  composite = (Composite) super.createDialogArea(parent);
        Label descriptionLabel = new Label(composite, SWT.LEAD|SWT.HORIZONTAL|SWT.WRAP);
        descriptionLabel.setText(desc.getEventDescription()); //$NON-NLS-1$
        
        ParameterDescription[] parameters = desc.getParameters();
        
        for (int i = 0; i < parameters.length; i++) {
            Composite comp = new Composite(parent, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.numColumns = 2;
//            layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
//            layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
//            layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
//            layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
            comp.setLayout(layout);
            comp.setLayoutData(new GridData(GridData.FILL_BOTH));
            
            Label nameLabel = new Label(comp, SWT.LEAD|SWT.HORIZONTAL);
            nameLabel.setText(parameters[i].getName());
            nameLabel.setToolTipText(parameters[i].getControlID());
            
            String controlID = parameters[i].getControlID();
            if (controlID.startsWith("expression-")) {
                
            } else if ("prim".equals(controlID)) {
                combo = new Combo(comp, SWT.READ_ONLY|SWT.DROP_DOWN);

                combo.setItems(new String[] { "foo", "bar" });
                combo.deselectAll();
                
                combo.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent e) {
                    }

                    public void widgetSelected(SelectionEvent e) {
                        if (combo.getSelectionIndex() >= 0) {
                            okButton().setEnabled(true);
                        } else {
                            okButton().setEnabled(false);
                        }
                    }
                });
                
            }
        }
        
        return composite;
    }

}
