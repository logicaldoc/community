package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the tags list with each tag count.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagsForm extends VLayout {

	private static final String SEARCHINHITS = "searchinhits";

	private ListGrid tags;

	private static TagsForm instance;

	private boolean admin = false;

	private DynamicForm vocabularyForm;

	private DynamicForm otherCharForm;

	public static TagsForm get() {
		if (instance == null)
			instance = new TagsForm(false, true);
		return instance;
	}

	/**
	 * @param admin Is for Admin mode
	 * @param searchInHits True if the search in hits must be shown
	 */
	public TagsForm(final boolean admin, final boolean searchInHits) {
		setMembersMargin(3);
		setWidth100();
		setHeight100();
		setOverflow(Overflow.AUTO);

		this.admin = admin;

		HLayout vocabulary = new HLayout();
		vocabulary.setMargin(5);
		vocabulary.setMembersMargin(10);		
		vocabulary.setHeight(1);

		vocabularyForm = new DynamicForm();
		vocabularyForm.setNumCols(9);
		vocabularyForm.setWidth(1);
		vocabularyForm.setHeight(1);
		List<FormItem> items = new ArrayList<FormItem>();
		String str = Session.get().getInfo().getConfig("tag.vocabulary");
		for (int i = 0; i < str.length(); i++) {
			final StaticTextItem item = ItemFactory.newStaticTextItem("" + i, "", "" + str.charAt(i));
			item.setShowTitle(false);
			item.setWidth(12);
			item.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					onLetterSelect((String) item.getValue());
				}
			});
			items.add(item);
		}
		vocabularyForm.setItems(items.toArray(new FormItem[0]));

		otherCharForm = new DynamicForm();
		otherCharForm.setWidth(1);

		PickerIcon searchPicker = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				if (!otherCharForm.validate())
					return;
				onLetterSelect(otherCharForm.getValueAsString("otherchar"));
			}
		});

		TextItem otherChar = ItemFactory.newTextItem("otherchar", "otherchar", null);
		otherChar.setRequired(true);
		otherChar.setWrapTitle(false);
		otherChar.setIcons(searchPicker);
		otherChar.setLength(1);
		otherChar.setWidth(50);

		FormItem searchinhits = ItemFactory.newYesNoSelectItem(SEARCHINHITS, SEARCHINHITS);
		searchinhits.setValue("false");

		if (searchInHits && !admin)
			otherCharForm.setItems(otherChar, searchinhits);
		else
			otherCharForm.setItems(otherChar);

		vocabulary.setMembers(vocabularyForm, otherCharForm);

		addMember(vocabulary);

		reloadTags(admin, null);
	}

	private void reloadTags(final boolean admin, String letter) {
		if (tags != null) {
			removeMember(tags);
		}

		ListGridField index = new ListGridField("index", " ", 10);
		index.setHidden(true);
		ListGridField word = new ListGridField("word", I18N.message("tag"));
		word.setWidth("*");
		ListGridField count = new ListGridField("count", I18N.message("count"), 60);
		tags = new ListGrid();
		tags.setEmptyMessage(I18N.message("notitemstoshow"));
		tags.setFields(index, word, count);
		tags.setSelectionType(SelectionStyle.SINGLE);
		tags.setDataSource(new TagsDS(letter, false, null, null));
		addMember(tags);

		tags.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				ListGridRecord record = event.getRecord();
				executeSearch(record);
			}
		});

		tags.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu(admin);
				event.cancel();
			}
		});
	}

	private void executeSearch(ListGridRecord record) {
		searchTag(record.getAttributeAsString("word"),
				new Boolean(otherCharForm.getValueAsString(SEARCHINHITS)).booleanValue());
	}

	private void showContextMenu(boolean admin) {
		Menu contextMenu = new Menu();

		MenuItem search = new MenuItem();
		search.setTitle(I18N.message("search"));
		search.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = tags.getSelectedRecord();
				executeSearch(selection);
			}
		});
		contextMenu.addItem(search);

		if (admin) {
			MenuItem rename = new MenuItem();
			rename.setTitle(I18N.message("rename"));
			rename.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

				public void onClick(MenuItemClickEvent event) {
					LD.askForValue(I18N.message("rename"), I18N.message("newtag"), "", new ValueCallback() {
						@Override
						public void execute(final String value) {
							if (value == null || "".equals(value.trim()))
								return;

							ListGridRecord selection = tags.getSelectedRecord();
							TagService.Instance.get().rename(selection.getAttribute("word"), value,
									new AsyncCallback<Void>() {
										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void arg) {
											Log.info(I18N.message("procinexecution"), I18N.message("taginexecution"));
											ListGridRecord selection = tags.getSelectedRecord();
											selection.setAttribute("word", value);
											onLetterSelect(value.substring(0, 1));
										}
									});
						}
					});
				}
			});
			contextMenu.addItem(rename);

			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
				public void onClick(MenuItemClickEvent event) {
					LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
						@Override
						public void execute(Boolean value) {
							if (value) {
								ListGridRecord selection = tags.getSelectedRecord();
								TagService.Instance.get().delete(selection.getAttribute("word"),
										new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable caught) {
												Log.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												Log.info(I18N.message("procinexecution"),
														I18N.message("taginexecution"));
												tags.removeSelectedData();
											}
										});
							}
						}
					});
				}
			});
			contextMenu.addItem(delete);
		}

		contextMenu.showContextMenu();
	}

	private void onLetterSelect(String letter) {
		reloadTags(admin, letter);
		tags.fetchData();
		tags.hideField("index");
	}

	/**
	 * Launches the search for one tag
	 */
	public static void searchTag(String word, boolean searchInHits) {
		MainPanel.get().selectSearchTab();
		SearchMenu.get().openTagsSection();
		TagsForm.get().onLetterSelect(word.substring(0, 1));
		GUISearchOptions options = new GUISearchOptions();
		options.setType(GUISearchOptions.TYPE_TAGS);
		options.setExpression(word);

		String hits = Session.get().getConfig("search.hits");
		if (hits != null)
			options.setMaxHits(Integer.parseInt(hits));

		if (searchInHits) {
			GUIDocument[] records = Search.get().getLastResult();
			Long[] ids = new Long[records.length];
			int i = 0;
			for (GUIDocument rec : records) {
				ids[i] = rec.getId();
				i++;
			}
			options.setFilterIds(ids);
		} else
			options.setFilterIds(null);

		Search.get().setOptions(options);
		Search.get().search();
	}
}