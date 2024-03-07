package com.logicaldoc.gui.frontend.client.document.signature;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailTab;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the digital signatures.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class DigitalSignaturePanel extends DocumentDetailTab {

	private static final String REASON = "reason";

	private VLayout container = new VLayout();

	private ListGrid list = null;

	public DigitalSignaturePanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addMember(container);
		refresh(document);
	}

	private void refresh(final GUIDocument document) {
		if (list != null)
			list.destroy();
		container.removeMembers(container.getMembers());

		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField date = new DateListGridField("date", "date");
		ListGridField signedBy = new UserListGridField("comment", "userId", "signedby");
		signedBy.setWidth("*");
		ListGridField reasonColumn = new ListGridField(REASON, I18N.message(REASON));
		reasonColumn.setWidth(250);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(null, document.getId(), "event.signed", null));
		list.setFields(date, signedBy, reasonColumn);

		TextItem reason = ItemFactory.newTextItem(REASON, null);
		reason.setWidth(400);
		reason.setRequired(true);
		reason.setWrapTitle(false);

		final CheckboxItem visualPositioning = new CheckboxItem();
		visualPositioning.setName("visualpositioning");
		visualPositioning.setTitle(I18N.message("visualpositioning"));
		visualPositioning.setDisabled(true);

		String url = Util.contextPath() + "export-keystore?cert=root&tenantId=" + Session.get().getTenantId();
		StaticTextItem rootCert = ItemFactory.newStaticTextItem("rootcertificate",
				"<a href='" + url + "' target='_blank'>" + I18N.message("downloadrootcert") + "</a>");
		rootCert.setRequired(true);
		rootCert.setWrap(false);
		rootCert.setColSpan(7);
		rootCert.setStartRow(true);

		final DynamicForm form = new DynamicForm();
		form.setWrapItemTitles(false);
		form.setWidth(1);
		form.setHeight(1);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(7);

		ButtonItem sign = new ButtonItem(I18N.message("signnow"));
		sign.setEndRow(false);
		sign.addClickHandler(event -> {
			if (!form.validate())
				return;

			if (Boolean.TRUE.equals(visualPositioning.getValueAsBoolean())) {
				VisualPositioningDigitalSignatureDialog dialog = new VisualPositioningDigitalSignatureDialog(
						Arrays.asList(document.getId()), form.getValueAsString(REASON));
				dialog.show();
			} else {
				LD.contactingServer();
				SignService.Instance.get().signDocuments(Arrays.asList(document.getId()), form.getValueAsString(REASON),
						1, null, null, null, new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								LD.clearPrompt();
								refresh(document);
							}
						});
			}
		});

		if (document.getFolder().hasPermission(GUIAccessControlEntry.PERMISSION_SIGN))
			form.setItems(sign, reason, visualPositioning, rootCert);
		else {
			form.setItems(rootCert);
		}

		container.addMember(list);
		container.addMember(form);

		SignService.Instance.get().isVisualSignatureEnabled(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Boolean enabled) {
				visualPositioning.setDisabled(!enabled);
			}
		});
	}
}