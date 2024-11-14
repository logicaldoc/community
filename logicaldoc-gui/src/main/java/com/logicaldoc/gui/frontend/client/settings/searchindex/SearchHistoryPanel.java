package com.logicaldoc.gui.frontend.client.settings.searchindex;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.UserHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.CopyCellClickHandler;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows search history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class SearchHistoryPanel extends VLayout {

	private static final String USERID = "userId";

	@Override
	protected void onDraw() {
		setWidth100();
		setHeight100();
		initGUI();
	}

	private void initGUI() {
		ColoredListGridField id = new ColoredListGridField("id");
		id.setHidden(true);

		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ColoredListGridField comment = new ColoredListGridField("comment", I18N.message("search"));
		comment.setWidth("*");
		comment.setCellFormatter((value, rcd, rowNum, colNum) -> {
			if (value == null)
				return null;
			String val = value.toString().replace("FulltextSearch[", "").replace(",expression=", ",<b>expression=")
					.replace(",userId=", "</b>,userId=").replace(",fields=", ",<b>fields=")
					.replace(",page=", "</b>,page=");
			if (val.endsWith("]"))
				val = val.substring(0, val.length() - 1);
			return val;
		});

		ListGridField userId = new ListGridField(USERID, I18N.message("userid"), 100);
		userId.setCanFilter(true);
		userId.setHidden(true);

		ListGridField userField = new UserListGridField("user", USERID, "user");
		userField.setCanFilter(true);
		userField.setAlign(Alignment.CENTER);

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new UserHistoryDS(Session.get().getTenantId(), null, "event.user.search", "Fulltext",
				Session.get().getConfigAsInt("gui.maxhistories")));
		list.setFields(id, date, userId, userField, comment);
		list.addCellDoubleClickHandler(new CopyCellClickHandler());

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		SpinnerItem maxItem = ItemFactory.newSpinnerItem("max", "display",
				Session.get().getConfigAsInt("gui.maxhistories"), 1, (Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		maxItem.addChangedHandler(changed -> refresh(list, maxItem.getValueAsInteger()));

		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		refresh.addClickHandler(click -> refresh(list, maxItem.getValueAsInteger()));

		buttons.addButton(refresh);
		buttons.addFormItem(maxItem);
		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(click -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(click -> GridUtil.print(list));

		buttons.addSeparator();

		Layout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(buttons);
		container.addMember(list);
		addMember(container);
	}

	private void refresh(RefreshableListGrid list, int max) {
		list.refresh(new UserHistoryDS(Session.get().getTenantId(), null, "event.user.search", "Fulltext", max));
	}
}