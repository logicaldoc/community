/**
 *
 * (c) Copyright LogicalDOC SRL 2024
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.logicaldoc.onlyoffice.entities;

import java.util.List;

public class CommentGroups {
    public List<String> getView() {
        return view;
    }

    public List<String> getEdit() {
        return edit;
    }

    public List<String> getRemove() {
        return remove;
    }

    private List<String> view;
    private List<String> edit;
    private List<String> remove;
    public CommentGroups() {

    }
    public CommentGroups(final List<String> viewParam, final List<String> editParam, final List<String> removeParam) {
        this.view = viewParam;
        this.edit = editParam;
        this.remove = removeParam;
    }
}