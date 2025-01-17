package com.logicaldoc.gui.frontend.client.docusign;

import java.util.Collection;

import com.google.gwt.user.client.Window;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.data.DocuSignEnvelopesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the user's envelopes.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class Envelopes extends com.smartgwt.client.widgets.Window {

	private ListGrid list;

	private SpinnerItem maxItem;

	public Envelopes() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("envelopes"));
		setWidth(Window.getClientWidth() / 2);
		setHeight(Window.getClientHeight() / 2);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setAutoSize(true);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> refresh());

		maxItem = ItemFactory.newSpinnerItem("max", "display", 20, 2, (Integer) null);
		maxItem.setHint(I18N.message("elements"));
		maxItem.setWidth(70);
		maxItem.setStep(10);
		maxItem.setMin(10);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.addChangedHandler(event -> refresh());

		toolStrip.addButton(refresh);
		toolStrip.addFormItem(maxItem);
		toolStrip.addFill();
		addItem(toolStrip);

		initGrid();

		addResizedHandler(event -> list.setHeight(getHeight() - 68));
	}

	private void initGrid() {
		if (list != null)
			removeItem(list);

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField subject = new ListGridField("subject", I18N.message("subject"));
		subject.setCanFilter(true);
		subject.setWidth("*");

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField expire = new DateListGridField("expire", "expireson");

		ListGridField updated = new DateListGridField("updated", "lastmodified");
		updated.setHidden(true);

		ListGridField status = new ListGridField("status", I18N.message("status"));
		status.setCanFilter(true);
		status.setWidth(70);

		list = new ListGrid();
		list.setWidth100();
		list.setHeight(getHeight());
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new DocuSignEnvelopesDS(Integer.parseInt(maxItem.getValueAsString())));
		list.setFields(id, subject, status, created, updated, expire);
		list.sort("created", SortDirection.DESCENDING);

		list.addDoubleClickHandler(event -> {
			LD.askForValue(I18N.message("id"), I18N.message("id"), list.getSelectedRecord().getAttributeAsString("id"),
					300, value -> {
						// Nothing to do
					});
			event.cancel();
		});

		list.addCellContextClickHandler(event -> {

			MenuItem inviteToChat = new MenuItem();
			inviteToChat.setTitle(I18N.message("signers"));
			inviteToChat.addClickHandler(evnt -> DocuSignService.Instance.get()
					.getSigners(list.getSelectedRecord().getAttributeAsString("id"), new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Collection<String> signers) {
							StringBuilder message = new StringBuilder();
							for (String signer : signers) {
								message.append("\n");
								message.append(signer);
							}
							SC.say(I18N.message("signers"), message.toString());
						}
					}));

			Menu contextMenu = new Menu();
			contextMenu.setItems(inviteToChat);
			contextMenu.showContextMenu();
			event.cancel();
		});

		addItem(list);
	}

	public void refresh() {
		initGrid();
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}