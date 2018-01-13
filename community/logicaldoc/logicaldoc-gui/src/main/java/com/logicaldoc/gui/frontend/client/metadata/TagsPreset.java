package com.logicaldoc.gui.frontend.client.metadata;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the tags list with each tag count.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class TagsPreset extends VLayout {

	private ListGrid tags;

	private ButtonItem addTag;

	public TagsPreset(String tagMode) {
		setMembersMargin(3);

		final DynamicForm form = new DynamicForm();

		final SelectItem mode = ItemFactory.newTagInputMode("mode", "inputmode");
		mode.setValue(tagMode);
		mode.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				SettingService.Instance.get().saveSettings(
						new GUIParameter[] { new GUIParameter(Session.get().getTenantName() + ".tag.mode", mode
								.getValueAsString()) }, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable t) {
								Log.serverError(t);
							}

							@Override
							public void onSuccess(Void arg) {
								Session.get()
										.getInfo()
										.setConfig(Session.get().getTenantName() + ".tag.mode", mode.getValueAsString());
								Log.info(I18N.message("settingssaved"), null);
							}
						});
			}
		});

		addTag = new ButtonItem();
		addTag.setTitle(I18N.message("addtag"));
		addTag.setAutoFit(true);
		addTag.setRequired(true);
		addTag.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				LD.askForValue(I18N.message("addtag"), I18N.message("tag"), "", new ValueCallback() {
					@Override
					public void execute(String value) {
						if (value != null && !"".equals(value))
							TagService.Instance.get().addTag(value, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg0) {
									Log.info(I18N.message("settingssaved"), null);
									reloadTags();
								}
							});
					}
				});
			}
		});

		form.setItems(mode, addTag);
		addMember(form);

		reloadTags();
	}

	private void reloadTags() {
		if (tags != null)
			removeMember(tags);

		tags = new ListGrid();
		tags.setEmptyMessage(I18N.message("notitemstoshow"));
		tags.setWidth(200);
		tags.setHeight(200);
		tags.setEmptyMessage(I18N.message("norecords"));
		tags.setSelectionType(SelectionStyle.SINGLE);
		ListGridField index = new ListGridField("index", " ", 10);
		index.setHidden(true);
		ListGridField word = new ListGridField("word", I18N.message("tag"));
		tags.setFields(index, word);
		tags.setDataSource(new TagsDS("preset", true, null, null));
		tags.setAutoFetchData(true);
		tags.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		addMember(tags);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("delete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = tags.getSelectedRecord();
				TagService.Instance.get().removeTag(selection.getAttributeAsString("word"), new AsyncCallback<Void>() {

					@Override
					public void onSuccess(Void arg0) {
						tags.removeSelectedData();
					}

					@Override
					public void onFailure(Throwable arg0) {
						Log.serverError(arg0);
					}
				});
			}
		});
		contextMenu.addItem(delete);

		contextMenu.showContextMenu();
	}
}