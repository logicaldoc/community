package com.logicaldoc.gui.common.client.widgets;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.selector.DocumentSelectorDialog;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * Allows the selection of a specific document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class DocumentSelector extends StaticTextItem {

	private Long documentId;

	private Collection<DocumentChangeListener> listeners = new ArrayList<>();

	public DocumentSelector(String name, List<FormItemIcon> additionalIcons) {
		if (name != null)
			setName(name);
		else
			setName("document");
		setTitle(I18N.message("document"));
		setWrapTitle(false);
		setValue("");
		setHintStyle("hint");

		if (additionalIcons != null && !additionalIcons.isEmpty())
			setWidth(180);

		PickerIcon search = new PickerIcon(PickerIcon.SEARCH, evnt -> new DocumentSelectorDialog() {

			@Override
			protected void onSelection(GUIDocument[] selection) {
				setDocument(selection[0]);
				close();
			}
		}.show());
		search.setWidth(12);
		search.setHeight(12);

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, event -> {
			clearValue();
			setDocument(null, null);
		});
		clear.setWidth(12);
		clear.setHeight(12);

		List<FormItemIcon> icons = new ArrayList<>();
		icons.add(search);
		icons.add(clear);
		if (additionalIcons != null && !additionalIcons.isEmpty()) {
			icons.addAll(additionalIcons);
		}
		setIcons(icons.toArray(new FormItemIcon[0]));

		addClickHandler(event -> {
			if (documentId != null)
				DocumentsPanel.get().openInFolder(documentId);
		});
	}

	public void setDocument(Long documentId, String fileName) {
		this.documentId = documentId;

		if (fileName != null && !(fileName.endsWith(" ") || fileName.endsWith("&nbsp;"))) {
			setValue(fileName);
			setTooltip(fileName);
		} else {
			setValue("");
			setTooltip("");
		}

		for (DocumentChangeListener listener : listeners) {
			listener.onChanged(getDocument());
		}
	}

	public void setDocument(GUIDocument document) {
		Long id = null;
		String fileName = null;

		if (document != null) {
			id = document.getId();
			fileName = document.getFileName();
		}

		setDocument(id, fileName);
	}

	public GUIDocument getDocument() {
		if (getDocumentId() != null) {
			GUIDocument document = new GUIDocument();
			document.setId(getDocumentId());
			document.setFileName(getDocumentName());
			return document;
		} else
			return null;
	}

	public Long getDocumentId() {
		return documentId;
	}

	public String getDocumentName() {
		return (String) getValue();
	}

	public void addDocumentChangeListener(DocumentChangeListener listener) {
		listeners.add(listener);
	}
}