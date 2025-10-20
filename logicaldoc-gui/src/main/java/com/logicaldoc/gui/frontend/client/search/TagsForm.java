package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * This panel shows the tags list with each tag count.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagsForm extends VLayout implements SearchObserver {

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
		setHeight100();
		setOverflow(Overflow.AUTO);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);

		if (!admin)
			Search.get().addObserver(this);

		this.admin = admin;

		HLayout vocabulary = new HLayout();
		vocabulary.setMargin(5);
		vocabulary.setMembersMargin(10);
		vocabulary.setHeight(1);

		vocabularyForm = new DynamicForm();
		vocabularyForm.setNumCols(9);
		vocabularyForm.setWidth(1);
		vocabularyForm.setHeight(1);
		List<FormItem> items = new ArrayList<>();
		String str = Session.get().getInfo().getConfig("gui.tag.vocabulary", "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		for (int i = 0; i < str.length(); i++) {
			final StaticTextItem item = ItemFactory.newStaticTextItem("" + i, "", "" + str.charAt(i));
			item.setShowTitle(false);
			item.setWidth(12);
			item.addClickHandler(event -> onLetterSelect((String) item.getValue()));
			items.add(item);
		}
		vocabularyForm.setItems(items.toArray(new FormItem[0]));

		otherCharForm = new DynamicForm();
		otherCharForm.setWidth(1);

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message("search"));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.addFormItemClickHandler(click -> {
			if (!otherCharForm.validate())
				return;
			onLetterSelect(otherCharForm.getValueAsString("otherchar"));
		});

		TextItem otherChar = ItemFactory.newTextItem("otherchar", null);
		otherChar.setRequired(true);
		otherChar.setWrapTitle(false);
		otherChar.setIcons(search);
		otherChar.setLength(1);
		otherChar.setWidth(50);

		FormItem searchinhits = ItemFactory.newToggleItem(SEARCHINHITS, I18N.message(SEARCHINHITS), false);

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

		tags.addCellDoubleClickHandler(event -> executeSearch(event.getRecord()));

		tags.addCellContextClickHandler(event -> {
			showContextMenu(admin);
			event.cancel();
		});
	}

	private void executeSearch(ListGridRecord rec) {
		searchTag(rec.getAttributeAsString("word"), Boolean.parseBoolean(otherCharForm.getValueAsString(SEARCHINHITS)));
	}

	private void showContextMenu(boolean admin) {
		Menu contextMenu = new Menu();

		MenuItem search = new MenuItem();
		search.setTitle(I18N.message("search"));
		search.addClickHandler(event -> executeSearch(tags.getSelectedRecord()));
		contextMenu.addItem(search);

		if (admin) {
			MenuItem rename = new MenuItem();
			rename.setTitle(I18N.message("rename"));
			rename.addClickHandler(
					event -> LD.askForValue(I18N.message("rename"), I18N.message("newtag"), "", value -> {
						if (value == null || "".equals(value.trim()))
							return;

						ListGridRecord selection = tags.getSelectedRecord();
						TagService.Instance.get().rename(selection.getAttribute("word"), value,
								new DefaultAsyncCallback<>() {
									@Override
									public void handleSuccess(Void arg) {
										GuiLog.info(I18N.message("procinexecution"), I18N.message("taginexecution"));
										ListGridRecord selection = tags.getSelectedRecord();
										selection.setAttribute("word", value);
										onLetterSelect(value.substring(0, 1));
									}
								});
					}));
			contextMenu.addItem(rename);

			MenuItem delete = new MenuItem();
			delete.setTitle(I18N.message("ddelete"));
			delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
				if (Boolean.TRUE.equals(confirm)) {
					ListGridRecord selection = tags.getSelectedRecord();
					TagService.Instance.get().delete(selection.getAttribute("word"), new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(Void arg) {
							GuiLog.info(I18N.message("procinexecution"), I18N.message("taginexecution"));
							tags.removeSelectedData();
						}
					});
				}
			}));
			contextMenu.addItem(new MenuItemSeparator());
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
	 * 
	 * @param word the tag to search
	 * @param searchInHits if the search must be done in the current hits
	 */
	public static void searchTag(String word, boolean searchInHits) {
		MainPanel.get().selectSearchTab();
		SearchMenu.get().openTagsSection();
		TagsForm.get().onLetterSelect(word.substring(0, 1));
		GUISearchOptions options = new GUISearchOptions();
		options.setType(GUISearchOptions.TYPE_TAGS);
		options.setExpression(word);
		options.setMaxHits(Search.get().getMaxHits());

		if (searchInHits)
			options.setFilterIds(
					Search.get().getLastResult().stream().map(doc -> doc.getId()).collect(Collectors.toList()));
		else
			options.setFilterIds(new ArrayList<>());

		Search.get().setOptions(options);
		Search.get().search();
	}

	@Override
	public void onOptionsChanged(GUISearchOptions options) {
		if (options.getType() == GUISearchOptions.TYPE_TAGS) {
			SearchMenu.get().openTagsSection();
			onLetterSelect(options.getExpression().substring(0, 1));
		}
	}

	@Override
	public void onSearchArrived() {
		// Do nothing
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