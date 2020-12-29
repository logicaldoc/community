package com.logicaldoc.gui.frontend.client.docusign;

import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.frontend.client.document.note.AbstractAnnotationsWindow;
import com.logicaldoc.gui.frontend.client.document.note.AnnotationCanvas;

/**
 * Canvas used to display the tabs for electronic signature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class ESignatureTabCanvas extends AnnotationCanvas {

	public ESignatureTabCanvas(GUIDocumentNote note, AbstractAnnotationsWindow annotationsWindow) {
		super(note, annotationsWindow, true);
	}

	@Override
	public void onEdit() {
		ESignatureTabEditor editor = new ESignatureTabEditor(note) {
			@Override
			void onSave() {
				super.onSave();
				ESignatureTabCanvas.this.setContents(note.getMessage());
				ESignatureTabCanvas.this.setBackgroundColor(note.getColor());
				ESignatureTabCanvas.this.setOpacity(note.getOpacity());
			}
		};
		editor.show();
	}
}
