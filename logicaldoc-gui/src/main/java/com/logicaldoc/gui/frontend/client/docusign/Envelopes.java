package com.logicaldoc.gui.frontend.client.docusign;

import java.util.Collection;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.DocuSignEnvelopesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
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
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		maxItem = ItemFactory.newSpinnerItem("max", "display", 20, 2, (Integer) null);
		maxItem.setHint(I18N.message("elements"));
		maxItem.setWidth(70);
		maxItem.setStep(10);
		maxItem.setMin(10);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});

		toolStrip.addButton(refresh);
		toolStrip.addFormItem(maxItem);
		toolStrip.addFill();
		addItem(toolStrip);

		initGrid();

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				list.setHeight(getHeight() - 68);
			}
		});
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

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				LD.askForValue(I18N.message("id"), I18N.message("id"),
						list.getSelectedRecord().getAttributeAsString("id"), 300, new ValueCallback() {
							@Override
							public void execute(final String value) {
							}
						});
				event.cancel();
			}
		});

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {

				MenuItem inviteToChat = new MenuItem();
				inviteToChat.setTitle(I18N.message("signers"));
				inviteToChat.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						DocuSignService.Instance.get().getSigners(list.getSelectedRecord().getAttributeAsString("id"),
								new AsyncCallback<Collection<String>>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(Collection<String> signers) {
										String message = "";
										for (String signer : signers)
											message += "\n" + signer;
										SC.say(I18N.message("signers"), message);
									}
								});
					}
				});

				Menu contextMenu = new Menu();
				contextMenu.setItems(inviteToChat);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});

		addItem(list);
	}

	public void refresh() {
		initGrid();
	}
}