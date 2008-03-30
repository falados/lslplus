package lslplus.editor;

import java.util.ResourceBundle;

import lslplus.LslPlusPlugin;
import lslplus.debug.LslLineBreakpoint;
import lslplus.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewerExtension5;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionAnnotation;
import org.eclipse.jface.text.source.projection.ProjectionAnnotationModel;
import org.eclipse.jface.text.source.projection.ProjectionSupport;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.texteditor.TextOperationAction;

/**
 * LSL (plus) text editor.
 */
public class LslPlusEditor extends TextEditor {
    public static final String ID = "lslplus.editor.LslPlusEditor"; //$NON-NLS-1$
    // borrowed from the Java editor example... should setup folding
    // regions for states, functions and handlers (instead).
    private class DefineFoldingRegionAction extends TextEditorAction {

        public DefineFoldingRegionAction(ResourceBundle bundle, String prefix, ITextEditor editor) {
            super(bundle, prefix, editor);
        }

        private IAnnotationModel getAnnotationModel(ITextEditor editor) {
            return (IAnnotationModel) editor.getAdapter(ProjectionAnnotationModel.class);
        }

        public void run() {
            ITextEditor editor = getTextEditor();
            ISelection selection = editor.getSelectionProvider().getSelection();
            if (selection instanceof ITextSelection) {
                ITextSelection textSelection = (ITextSelection) selection;
                if (!textSelection.isEmpty()) {
                    IAnnotationModel model = getAnnotationModel(editor);
                    if (model != null) {

                        int start = textSelection.getStartLine();
                        int end = textSelection.getEndLine();

                        try {
                            IDocument document = editor.getDocumentProvider().getDocument(
                                    editor.getEditorInput());
                            int offset = document.getLineOffset(start);
                            int endOffset = document.getLineOffset(end + 1);
                            Position position = new Position(offset, endOffset - offset);
                            model.addAnnotation(new ProjectionAnnotation(), position);
                        } catch (BadLocationException x) {
                            // ignore
                        }
                    }
                }
            }
        }
    }

    /** The projection support */
    private ProjectionSupport fProjectionSupport;

    /**
     * Create an instance of the editor.
     */
    public LslPlusEditor() {
        super();
    }

    /**
     * The <code>LslPlusEditor</code> implementation of this
     * <code>AbstractTextEditor</code> method extend the actions to add those
     * specific to the receiver
     */
    protected void createActions() {
        super.createActions();

        IAction a = new TextOperationAction(Messages.getResourceBundle(),
                "ContentAssistProposal.", this, ISourceViewer.CONTENTASSIST_PROPOSALS); //$NON-NLS-1$
        a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        setAction("ContentAssistProposal", a); //$NON-NLS-1$

        a = new TextOperationAction(Messages.getResourceBundle(),
                "ContentAssistTip.", this, ISourceViewer.CONTENTASSIST_CONTEXT_INFORMATION); //$NON-NLS-1$
        a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
        setAction("ContentAssistTip", a); //$NON-NLS-1$

        a = new DefineFoldingRegionAction(Messages.getResourceBundle(),
                "DefineFoldingRegion.", this); //$NON-NLS-1$
        setAction("DefineFoldingRegion", a); //$NON-NLS-1$
        
    }

    /**
     * Save the contents of the editor.
     * 
     * @param monitor the progress monitor
     */
    public void doSave(IProgressMonitor monitor) {
        super.doSave(monitor);
        LslPlusPlugin.getDefault().errorStatusChanged();
    }

    /**
     * doSaveAs specialization of the AbstractTextEditor's doSaveAs()...
     */
    public void doSaveAs() {
        super.doSaveAs();
        LslPlusPlugin.getDefault().errorStatusChanged();
    }

    protected void editorContextMenuAboutToShow(IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);
        addAction(menu, "ContentAssistProposal"); //$NON-NLS-1$
        addAction(menu, "ContentAssistTip"); //$NON-NLS-1$
        addAction(menu, "DefineFoldingRegion"); //$NON-NLS-1$
    }

    /**
     * Adapt this editor to the required type, if possible.
     * @param required the required type
     * @return an adapter for the required type or <code>null</code>
     */
    public Object getAdapter(Class required) {

        if (fProjectionSupport != null) {
            Object adapter = fProjectionSupport.getAdapter(getSourceViewer(), required);
            if (adapter != null)
                return adapter;
        }

        return super.getAdapter(required);
    }

    protected void initializeEditor() {
        super.initializeEditor();
        setSourceViewerConfiguration(new LslSourceViewerConfiguration());
    }

    protected ISourceViewer createSourceViewer(Composite parent, IVerticalRuler ruler, int styles) {
        fAnnotationAccess = createAnnotationAccess();
        fOverviewRuler = createOverviewRuler(getSharedColors());

        ISourceViewer viewer = new ProjectionViewer(parent, ruler, getOverviewRuler(),
                isOverviewRulerVisible(), styles);

        // ensure decoration support has been created and configured.
        getSourceViewerDecorationSupport(viewer);

        return viewer;
    }

    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        ProjectionViewer viewer = (ProjectionViewer) getSourceViewer();
        fProjectionSupport = new ProjectionSupport(viewer, getAnnotationAccess(), getSharedColors());
        fProjectionSupport
                .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.error"); //$NON-NLS-1$
        fProjectionSupport
                .addSummarizableAnnotationType("org.eclipse.ui.workbench.texteditor.warning"); //$NON-NLS-1$
        fProjectionSupport.install();
        viewer.doOperation(ProjectionViewer.TOGGLE);

        this.getVerticalRuler().getControl().addMouseListener(new MouseListener() {

            public void mouseDoubleClick(MouseEvent e) {
                IResource resource = (IResource) getEditorInput().getAdapter(IResource.class);
                
                if (resource != null) {
                    try {
                        new LslLineBreakpoint(resource, 
                                getVerticalRuler().toDocumentLineNumber(e.y) + 1);
                    } catch (DebugException e1) {
                        Util.log(e1, e1.getLocalizedMessage());
                    }
                } else {
                    Util.log("resource is null, can't create breakpoint");
                }
            }

            public void mouseDown(MouseEvent e) {
            }

            public void mouseUp(MouseEvent e) {
            }
            
        });
        // IAnnotationModel m = viewer.getAnnotationModel();
        //		
        // IDocument d = viewer.getDocument();

        // try {
        // int offset = d.getLineOffset(2);
        // int len = d.getLineLength(2);
        // m.addAnnotation(new
        // Annotation("org.eclipse.ui.workbench.texteditor.error", true,
        // "Error"),
        // new Position(offset,len));
        // } catch (BadLocationException e) {
        // Util.log(e,"bad location");
        // }

        // m.addAnnotationModelListener(new IAnnotationModelListener() {
        //
        // public void modelChanged(IAnnotationModel model) {
        // Util.log("Model changed!");
        // }
        //			
        // });
    }

    protected void adjustHighlightRange(int offset, int length) {
        ISourceViewer viewer = getSourceViewer();
        if (viewer instanceof ITextViewerExtension5) {
            ITextViewerExtension5 extension = (ITextViewerExtension5) viewer;
            extension.exposeModelRange(new Region(offset, length));
        }
    }
}
