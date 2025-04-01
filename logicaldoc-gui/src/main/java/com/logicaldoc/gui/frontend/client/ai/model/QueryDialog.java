package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to input data and obtain a prediction from the AI
 * model.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class QueryDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private ButtonItem query;

	private GUIModel model;

	private ListGrid resultGrid;

	public QueryDialog(GUIModel model) {
		this.model = model;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("querymodel2", model.getLabel()));
		setWidth(500);
		setHeight(350);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		query = new ButtonItem();
		query.setTitle(I18N.message("querymodel"));
		query.setAutoFit(true);
		query.setStartRow(true);
		query.addClickHandler(event -> onQuery());

		List<FormItem> items = new ArrayList<>();
		for (String feature : model.getFeaturesList()) {
			TextItem item = ItemFactory.newTextItem(feature, null);
			item.setRequired(true);
			items.add(item);
		}
		items.add(query);

		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(items.toArray(new FormItem[0]));
		form.setHeight("51%");
		form.setShowResizeBar(true);

		prepareResultGrid();

		VLayout content = new VLayout();
		content.setWidth100();
		content.setMembers(form, resultGrid);

		addItem(content);
	}

	private void prepareResultGrid() {
		resultGrid = new ListGrid();
		resultGrid.setEmptyMessage(I18N.message("noresults"));
		resultGrid.setShowAllRecords(true);
		resultGrid.setAutoFetchData(true);
		resultGrid.setWidth100();
		resultGrid.setHeight100();
		resultGrid.setSelectionType(SelectionStyle.SINGLE);
		resultGrid.setShowRecordComponents(true);
		resultGrid.setShowRecordComponentsByCell(true);
		resultGrid.setCanFreezeFields(true);
		resultGrid.setFilterOnKeypress(true);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setAutoFitWidth(true);
		name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		
		ListGridField score = new ListGridField("score", I18N.message("score"));
		
		ListGridField value = new ListGridField("value", I18N.message("value"));
		value.setAutoFitWidth(true);
		value.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		resultGrid.setFields(name, value, score);
	}

	private void onQuery() {
		query.setDisabled(true);
		AIService.Instance.get().query(model.getId(),
				model.getFeaturesList().stream().map(f -> form.getValueAsString(f)).collect(Collectors.toList()),
				new DefaultAsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						query.setDisabled(false);
					}

					@Override
					public void onSuccess(List<GUIQueryResult> answers) {
						query.setDisabled(false);
						resultGrid.setData(new RecordList(answers.stream().map(answer -> {
							ListGridRecord rec = new ListGridRecord();
							rec.setAttribute("name", answer.getName());
							rec.setAttribute("score", answer.getScore());
							rec.setAttribute("value", answer.getValue());
							return rec;
						}).collect(Collectors.toList())));
					}

				});
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