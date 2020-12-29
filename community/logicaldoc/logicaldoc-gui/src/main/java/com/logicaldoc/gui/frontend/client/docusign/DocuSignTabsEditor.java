package com.logicaldoc.gui.frontend.client.docusign;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.note.AbstractAnnotationsWindow;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to define the placehoders of a document where the
 * recipients have to sign
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class DocuSignTabsEditor extends AbstractAnnotationsWindow {

	public DocuSignTabsEditor(GUIDocument doc) {
		super(doc, null, Arrays.asList("esig-signhere", "esig-text", "esig-date", "esig-company", "esig-datesigned",
				"esig-title", "esig-fullname", "esig-envelopeid", "esig-email", "esig-emailaddress"));
		setTitle(I18N.message("docusigntabs") + " - " + doc.getFileName());
	}

	@Override
	protected void onNotesSaved() {
		destroy();
	}

	@Override
	protected Canvas getAnnotationCanvas(GUIDocumentNote note) {
		return new ESignatureTabCanvas(note, DocuSignTabsEditor.this);
	}

	@Override
	protected void prepareAdditionalActions(ToolStrip toolStrip) {
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtab"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.askforValue("addtab", "addtab", "esig-signhere", ItemFactory.newDocuSignTabType("esig-signhere"),
						new ValueCallback() {

							@Override
							public void execute(String value) {
								captureNotesPosition();

								GUIDocumentNote note = new GUIDocumentNote();
								note.setDocId(document.getId());
								note.setUserId(Session.get().getUser().getId());
								note.setUsername(Session.get().getUser().getFullName());
								note.setMessage(I18N.message("annotation.type." + value));
								note.setPage((Integer) pageCursor.getValue());
								note.setType(value);
								notes.add(note);

								ESignatureTabCanvas tabCanvas = new ESignatureTabCanvas(note, DocuSignTabsEditor.this);
								pageImage.addCanvas(tabCanvas);
								tabCanvas.onEdit();
							}
						});
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
	}
}