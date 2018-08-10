package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the signatures.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class SignaturePanel extends DocumentDetailTab {

	private VLayout container = new VLayout();

	private ListGrid list = null;

	public SignaturePanel(final GUIDocument document) {
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

		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField comment = new ListGridField("comment", I18N.message("signedby"));

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(null, document.getId(), "event.signed", null));
		list.setFields(date, comment);

		HLayout formLayout = new HLayout();
		formLayout.setMembersMargin(4);
		formLayout.setWidth100();
		formLayout.setHeight(80);

		TextItem reason = ItemFactory.newTextItem("reason", "reason", null);
		reason.setWidth(400);
		reason.setRequired(true);
		reason.setWrapTitle(false);

		TextItem password = ItemFactory.newPasswordItem("password", "password", null);
		password.setRequired(true);
		password.setWrapTitle(false);

		String url = Util.contextPath() + "export-keystore?cert=true&tenantId=" + Session.get().getTenantId();
		StaticTextItem rootCert = ItemFactory.newStaticTextItem("rootcertificate", "rootcertificate", "<a href='" + url
				+ "'>" + I18N.message("downloadrootcert") + "</a>");
		rootCert.setRequired(true);
		rootCert.setWrap(false);
		rootCert.setColSpan(5);

		final DynamicForm form = new DynamicForm();
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(5);

		ButtonItem sign = new ButtonItem(I18N.message("signnow"));
		sign.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				if (!form.validate())
					return;
				ContactingServer.get().show();
				SignService.Instance.get().signDocuments(new long[] { document.getId() },
						form.getValueAsString("reason"), form.getValueAsString("password"), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								ContactingServer.get().hide();
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								ContactingServer.get().hide();
								refresh(document);
							}
						});
			}
		});

		if (document.getFolder().hasPermission(Constants.PERMISSION_SIGN))
			form.setItems(reason, password, sign, rootCert);
		else {
			formLayout.setHeight(50);
			form.setItems(rootCert);
		}
		formLayout.addMember(form);

		container.addMember(list);
		container.addMember(formLayout);
	}
}