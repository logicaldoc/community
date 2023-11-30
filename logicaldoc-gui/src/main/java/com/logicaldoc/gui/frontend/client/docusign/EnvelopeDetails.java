package com.logicaldoc.gui.frontend.client.docusign;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocuSignSettings;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.SearchPanel;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to collect the details of an envelope
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class EnvelopeDetails extends Window {

	private IButton submitButton;

	private DynamicForm form;

	private GUIDocument[] documents = new GUIDocument[0];

	public EnvelopeDetails() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("envelope"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		documents = MainPanel.get().isOnDocumentsTab() ? DocumentsPanel.get().getDocumentsGrid().getSelectedDocuments()
				: SearchPanel.get().getDocumentsGrid().getSelectedDocuments();

		submitButton = new IButton(I18N.message("submit"));
		submitButton.addClickHandler(event -> onSubmit());

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);
		layout.addMember(submitButton);

		addItem(layout);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		List<FormItem> items = new ArrayList<>();

		TextItem subject = ItemFactory.newTextItem("subject", I18N.message("newdocumentstosign"));
		subject.setRequired(true);
		subject.setWidth(200);
		items.add(subject);

		TextItem message = ItemFactory.newTextItem("message", null);
		message.setWidth(350);
		items.add(message);

		DateItem expire = ItemFactory.newDateItem("expire", "expireson");
		items.add(expire);

		int i = 0;
		for (GUIDocument document : documents) {
			StaticTextItem docItem = ItemFactory.newStaticTextItem("doc" + i++, " ",
					"&bull; " + document.getFileName());
			docItem.setShowTitle(false);
			docItem.setColSpan(2);

			FormItemIcon editTabs = new FormItemIcon();
			editTabs.setSrc("[SKIN]/formedit.png");
			editTabs.setPrompt(I18N.message("edittabs"));
			editTabs.setWidth(12);
			editTabs.setHeight(12);
			editTabs.addFormItemClickHandler(event -> new DocuSignTabsEditor(document).show());
			docItem.setIcons(editTabs);

			items.add(docItem);
		}

		form.setItems(items.toArray(new FormItem[0]));
	}

	public void onSubmit() {
		if (!form.validate())
			return;

		List<Long> docIds = new ArrayList<>();
		for (GUIDocument document : documents)
			docIds.add(document.getId());

		LD.contactingServer();
		DocuSignService.Instance.get().validateEnvelope(docIds, new AsyncCallback<Collection<GUIDocument>>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Collection<GUIDocument> docs) {
				LD.clearPrompt();
				if (!docs.isEmpty()) {
					StringBuilder message = new StringBuilder(I18N.message("providesignheretabfordocs") + ": <ul>");
					for (GUIDocument doc : docs) {
						message.append("\n<li>");
						message.append(doc.getFileName());
						message.append("</li>");
					}
					message.append("\n</ol>");
					SC.warn(I18N.message("error"), message.toString());
				} else {
					GUIDocuSignSettings settings = new GUIDocuSignSettings();
					settings.setDocumentIds(docIds);
					settings.setExpire((Date) form.getValue("expire"));
					settings.setMessage(form.getValueAsString("message"));
					settings.setSubject(form.getValueAsString("subject"));

					LD.contactingServer();
					DocuSignService.Instance.get().sendEnvelope(settings, new AsyncCallback<String>() {

						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(String envelopeId) {
							LD.clearPrompt();
							SC.say(I18N.message("sentenvelope", envelopeId));
							destroy();
						}
					});
				}

			}
		});
	}
}