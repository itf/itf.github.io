function loadFile(path, success, error)
{ //from https://stackoverflow.com/a/18278346/5881930
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function()
    {
        if (xhr.readyState === XMLHttpRequest.DONE) {
            if (xhr.status === 200) {
                if (success)
                    success(xhr.responseText);
            } else {
                if (error)
                    error(xhr);
            }
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}


function run_from_yaml_file(yaml_file, subs) 
{
    function run_yaml_with_subs(story_yaml){
        run_yaml(story_yaml, subs);
    }
    loadFile(yaml_file, run_yaml_with_subs, console.log)
}

//require(['text!./story.yaml', 'js-yaml'], function (story_yaml, yaml) {
//Assumes jsyaml has been called previously
function run_yaml(story_yaml, subs)
{
   //Global variables
    'use strict';
    var story = jsyaml.load(story_yaml);
    var start_text = story.begin.text;
    var substitute_text_dictionary = subs;
    var previous_substituted_text = [];
    var new_substituted_text = [];
    var html_story = document.getElementById("story");
    
    var flags = {}; //Flags set by words. Permanent
    var text_flags = {}; // Flags set by text Temporary
    var temporary_flags = {}
    var word_keys = {}
    
    var permanent_styles_list = []; //Defines which styles for the body are permanent ones
    var temporary_styles_list = ["flash-dark", "flash-red", "get-dark", "flash-blood"]; //Defines which styles for the body are temporary ones, such as dark flashes.
    
    var p_styles = []; // Permanent styles applied to the body
    var t_styles = []; // Temporary styles applied to the body
    
    var previous_states=[]; // For undo.
    
    var timed_events = {};
    
    //For debugging: 
    var html_story_yaml = document.getElementById("story_yaml");
    var editor;
    
    
    
    ///Helper functions for the story
    var get_substitute_text = function(story_dictionary, text_keyword, flags, text_flags, timed_events){
        var text = story_dictionary[text_keyword];
        if (!text){
                return text_keyword;
        }
        if (text.constructor !== Array){
            text = [text];
        }
        var array_text = text;
        for (var t in array_text){
            text = array_text[t];
            if (!text.cond || conditions_met(text.cond, flags, text_flags, timed_events)){
                return text.text;
            }

        }
        
        return ".";
    };
    
    var perform_action_text = function(story_dictionary, text_keyword, flags, text_flags, timed_events){
        var text = story_dictionary[text_keyword];
        if (!text){
                return; 
        }
        var actions = get_action_function_array(text, story_dictionary, flags, text_flags, undefined, timed_events, undefined);
        for (var f in actions){
            actions[f]();
        }
        
    }
    
    
    var substitute_text = function (text_to_substitute, 
                                     substitute_dictionary, 
                                     story_dictionary, 
                                     previous_substituted_text, 
                                     new_substituted_text, 
                                     parent_dummy_span,
                                     flags,
                                     text_flags,
                                     present_text_keys,
                                     timed_events) {
        for (var key in substitute_dictionary){
            var text_keyword = substitute_dictionary[key]
            var to_substitute = '$'+key+'$';
            var contains_string = text_to_substitute.includes(to_substitute);

            if (contains_string){
                var substitute_text_content = get_substitute_text(story_dictionary, text_keyword, flags, text_flags, timed_events);
                add_flags(text_keyword,text_flags);
                perform_action_text(story_dictionary, text_keyword, flags, text_flags, timed_events);
                present_text_keys.push(text_keyword);
                new_substituted_text[key] = substitute_text_content;
                var new_text_html = get_dummy_word();
                new_text_html.id = key;
                new_text_html.innerHTML = substitute_text_content;
                if(substitute_text_content != previous_substituted_text[key]){
                    if (previous_substituted_text[key]){
                        new_text_html.className = 'substituting-text '
                    }
                    else{
                        new_text_html.className = 'new-text '
                    }
                }
                else{
                    new_text_html.className = 'old-text '
                }
                var substitute_text_with_html =  new_text_html.outerHTML;
                if (parent_dummy_span){
                    var span_tags = parent_dummy_span.split("$");
                    substitute_text_with_html = span_tags[1]+substitute_text_with_html+span_tags[0];
                }
                new_text_html.innerHTML="$";
                var new_dummy_span = new_text_html.outerHTML;
                substitute_text_with_html = substitute_text(substitute_text_with_html, 
                                                            substitute_dictionary, 
                                                            story_dictionary,  
                                                            previous_substituted_text, 
                                                            new_substituted_text, 
                                                            new_dummy_span,
                                                            flags,
                                                            text_flags,
                                                            present_text_keys,
                                                            timed_events);
                text_to_substitute = text_to_substitute.split(to_substitute).join(substitute_text_with_html);
            }
        }
        return text_to_substitute;
    };

    /// Helper functions for generating HTML
    

    var action_click_word = function(word_dict_object, text_dict, flags, text_flags, key, timed_events, word_text){
        var function_array = [];
        if (word_dict_object != null && word_dict_object.click){
            var click_array = word_dict_object.click;
            function_array = get_action_function_array(click_array, text_dict, flags, text_flags, key, timed_events, word_text);
        }
        if (function_array.length == 0){
            return null;
        }
        else{
            function_array.push(generate_story);
            return function_array_to_function(function_array);
        }
    }
    
    var action_auto_word = function(word_dict_object, text_dict, flags, text_flags, key, timed_events, word_text){
        var function_array = [];
        if (word_dict_object != null && word_dict_object.auto){
            var auto_array = word_dict_object.auto;
            function_array = get_action_function_array(auto_array, text_dict, flags, text_flags, key, timed_events, word_text);
        }
        if (function_array.length == 0){
            return null;
        }
        else{
            function_array.push(()=>generate_story());
            return function_array_to_function(function_array);
        }
    }
    
    var get_style = function(styles_array, text_dict, flags, text_flags, key, timed_events){
        if (styles_array.constructor !== Array){
            styles_array = [styles_array];
        }
        for (var s in  styles_array){
            var styles_and_cond = styles_array[s];
            if (conditions_met(styles_and_cond, flags, text_flags, timed_events)){
                var styles = styles_and_cond.s;
                if (styles.constructor !== Array){
                    styles = [styles];
                }
                return styles;
            }
        }
                
        
    }
    
    var get_action_function_array = function(actions_conds_array, text_dict, flags, text_flags, key, timed_events, word_text){ //Key and timed events are only used for timed events
        var function_array = [];
        if (actions_conds_array.constructor !== Array){
            actions_conds_array = [actions_conds_array];
        }
        for (var s in  actions_conds_array){
            var actions = actions_conds_array[s];
            if ( (!actions.cond || conditions_met(actions.cond, flags, text_flags, timed_events)) && is_useful_word(actions, text_dict, flags, text_flags, timed_events, key)){
                if (actions.rm_flags){
                    function_array.push(()=>remove_flags(actions.rm_flags, flags, timed_events));
                }
                function_array.push(()=>add_to_substitute_dict(actions.subs, text_dict));
                if (actions.flags){
                    function_array.push(()=>add_flags(actions.flags, flags));
                }
                if (actions.tflags){
                    function_array.push(()=>add_flags(actions.tflags, temporary_flags));
                }
                if (actions.body_class){
                    function_array.push(()=>add_styles_to_body(actions.body_class));
                }

                if (actions.undo){
                    function_array.push(()=>undo_story( ));
                }
                
                if (actions.e && actions.time){
                    function_array.push(()=>add_end_of_timed_event(actions.e, actions.time, key, timed_events, word_text, actions.ce));
                }

                break;
            }
        }
        return function_array;
    }

    var dummy_word = document.createElement('span'); //To not create multiple word elements
    var get_dummy_word = function(){
        while(dummy_word.attributes.length > 0)
            dummy_word.removeAttribute(dummy_word.attributes[0].name);
        return dummy_word;
    }

    var get_word_text = function(key, dictionary_word){
        if  (dictionary_word == null) {
            return key;
        }
        var word = dictionary_word['word'];
        if (typeof word == 'undefined') {
            return key;
        }
        if (typeof word == 'string'){
            return word;
        }
        else if (word.constructor === Array){
            if (word.length == 3){
                var word_inner = "";
                for (var w = 0 ; w<3; w++){
                    var span_w =  get_dummy_word();
                    span_w.innerHTML = word[w];
                    word_inner += span_w.outerHTML;
                }
                var word_html = get_dummy_word();
                word_html.classList.add('cycle3');
                word_html.innerHTML = word_inner;
                return word_html.outerHTML;
                
            }
        }
    }
    
    var substitute_words = function (html_story, dictionary, text_dict, flags, text_flags, timed_events){
        var original_text = html_story.innerHTML;
        var text_to_substitute = html_story.innerHTML;
        var keys = [];
        for (var key in dictionary){
            var to_substitute = '/'+key+'/';
            var contains_string = text_to_substitute.includes(to_substitute);
            if (contains_string){
                var word_text = get_word_text(key, dictionary[key]);
                keys.push(key);
                var word = get_dummy_word();
                word.id = key;
                word.classList.add(key);

                word.innerHTML = word_text;
                var substitute =  word.outerHTML;
                text_to_substitute = text_to_substitute.split(to_substitute).join(substitute);
                html_story.innerHTML = text_to_substitute;
            }  
        }

        add_flags(keys, word_keys)
        add_interactivity_to_words(keys, dictionary, text_dict, flags, text_flags, timed_events);
        add_auto_events_to_words(keys, dictionary, text_dict, flags, text_flags, timed_events);
        add_styles_to_words(keys, dictionary, text_dict, flags, text_flags, timed_events);
    }

    
    var add_interactivity_to_words = function(word_keys, dictionary, text_dict, flags, text_flags, timed_events){
        for (var index in word_keys){
            //add on click events
            var key = word_keys[index];
            var word_text = get_word_text(key, dictionary[key]);
            var words_html =  document.getElementsByClassName(key); //Supports multiple words
            for (let i = 0; i < words_html.length; i++){
                var word_html = words_html[i];
                var word_click_function = action_click_word(dictionary[key], text_dict, flags, text_flags, key, timed_events, word_text);
          
                if (word_click_function){
                    word_html.classList.add('clickable-word');
                    word_html.addEventListener("click", word_click_function, false);
                }
            }
        }
    }
    
    var add_auto_events_to_words = function(word_keys, dictionary, text_dict, flags, text_flags, timed_events){
        for (var index in word_keys){
            //add on click events
            var key = word_keys[index];
            var word_text = get_word_text(key, dictionary[key]);
            var word_html = document.getElementById(key)
            var word_auto_function = action_auto_word(dictionary[key], text_dict, flags, text_flags, key, timed_events, word_text);
            if (word_auto_function){
                word_auto_function();
            }
        }
    }
    
    var add_styles_to_words = function(word_keys, dictionary, text_dict, flags, text_flags, timed_events){
        for (var index in word_keys){
            var key = word_keys[index];
            var word_dict_object = dictionary[key];
            var word_html = document.getElementById(key)
            if (word_dict_object != null && word_dict_object.style){
                var style_array = word_dict_object.style;
                var styles = get_style(style_array, text_dict, flags, text_flags, key, timed_events);
                if (styles){
                    for (var s in styles){
                        word_html.classList.add(styles[s]);

                    }
                }
            }
        }
    }

    var remove_markers = function(text){
        text = text.replace(/\$.*?\$/g, '');
        return text;
    }

    var proccess_words = function(html_story, text, story_dict, text_dict, flags, text_flags, timed_events){
        var text = remove_markers(text);
        html_story.innerHTML = text;
        substitute_words(html_story, story_dict, text_dict, flags, text_flags, timed_events);
    }
    
    var clear_class_body = function(){
        var body = document.getElementById("text-background");
        body.className = " ";
    }
            
    
    var copy_dict = function(dict_to_copy_from, dict_to_copy_to){    
        Object.keys(dict_to_copy_from).forEach(function(key) {
         dict_to_copy_to[ key ] = dict_to_copy_from[ key ];
        }); 
    }
    
    var apply_styles_to_el = function(temporary_styles_list, permanent_styles_list, el){
        var class_list = el.classList;
        var length_classes = class_list.length;
        for(var i =0; i<length_classes; i++){
            var elClass = class_list[i];
            if (!permanent_styles_list.includes(elClass)){
                el.classList.remove(elClass);
            }
        }
        add_classes_to_el(temporary_styles_list, el);
        add_classes_to_el(permanent_styles_list, el);
        temporary_styles_list.length=0;
    }
    
    var add_classes_to_el = function(classes_list, el){
        for (var s in classes_list){
            var style = classes_list[s]; 
            add_class(style, el);
        }

    }
    


    // Actions
    
    var undo_story = function(){
        if (previous_states.length>1){
            var old_state = previous_states.shift();
            old_state = previous_states.shift();
            text_flags = old_state[0];
            flags = old_state[1];
            substitute_text_dictionary = old_state[2];
            previous_substituted_text = old_state[3];
            new_substituted_text = old_state[4];
            p_styles = old_state[5];
            t_styles = old_state[6];
            timed_events = old_state[7];
            temporary_flags = old_state[8];
        }
    }

    var add_to_substitute_dict =  function(substitute_dict_object, text_dict){
        for (var s in substitute_dict_object){
            text_dict[s] = substitute_dict_object[s];
        }
    }
    
    var add_flags =  function(flags_from_word, flags){
        if (flags_from_word.constructor !== Array){
            flags_from_word = [flags_from_word];
        }
        for (var f in flags_from_word){
            var flag = flags_from_word[f];
            var value = 0;
            var flag_name;
            if (typeof flag == 'string'){
                flag_name = flag;
                value = 1;
            }
            else{
                for (flag_name in flag){
                    value = flag[flag_name]
                }
            }
            if (flags[flag_name]){
                flags[flag_name] += value;
            }     
            else{
                flags[flag_name] = value;
            }   
        }
    };
    
    var remove_flags =  function(flags_to_remove, flags, timed_events){
        if (flags_to_remove.constructor !== Array){
            flags_to_remove = [flags_to_remove];
        }
        for (var f in flags_to_remove){
            delete flags[flags_to_remove[f]];
            delete timed_events[flags_to_remove[f]];
        }
    
    }
    
    var decrease_temporary_flags =  function(temporary_flags){
        for (var f in temporary_flags){
            temporary_flags[f]-=1;
            if (temporary_flags[f]==0){
                delete temporary_flags[f];
            }
        }
    
    }
    
    
    
    var add_styles_to_body = function(styles){
        if (styles.constructor !== Array){
            styles = [styles];
        }
        for (var s in styles){
            var style = styles[s];
            if (permanent_styles_list.includes(style)){
                add_permanent_style_to_body(style);
            }
            if (temporary_styles_list.includes(style)){
                add_temporary_style_to_body(style);
            }
        }
    }
    
    var add_end_of_timed_event = function(endings, time, key, timed_events, word_text, condition_end){
        if (timed_events[key]){ //It means that the timed event already exists.
            return;
        }
        //else, let's add it.
        timed_events[key] = new timed_event_word(key, time, endings, word_text, condition_end);
        
    }
    
    
    // Actions helper
    var add_permanent_style_to_body = function(style){
        p_styles.push(style);
    }
    
    var add_temporary_style_to_body = function(style){
        t_styles.push(style);
    }
        
    var add_class = function(class_name, el){
        setTimeout(()=>el.classList.add(class_name), 10 );
    }

    var add_flags =  function(flags_from_word, flags){
        if (flags_from_word.constructor !== Array){
            flags_from_word = [flags_from_word];
        }
        for (var f in flags_from_word){
            var flag = flags_from_word[f];
            var value = 0;
            var flag_name;
            if (typeof flag == 'string'){
                flag_name = flag;
                value = 1;
            }
            else{
                for (flag_name in flag){
                    value = flag[flag_name]
                }
            }
            if (flags[flag_name]){
                flags[flag_name] += value;
            }     
            else{
                flags[flag_name] = value;
            }   
        }
    };
    
    var is_useful_word = function(click_action, substitute_text_dictionary, flags, text_flags, timed_events, key){
        if (text_flags["END"] && !click_action.end){
            return false;
        }
        if (click_action.m){
            return true;
        }
        if (click_action.e && click_action.time && !timed_events[key]){
            return true; //means that it is a timed event with some time of ending
        }
        var subs = click_action.subs;
        if (subs){
            for (var s in subs){
                if (substitute_text_dictionary[s] != subs[s]){
                    return true;
                }
            }
        }
        
        var flags_from_word = click_action.flags;
        var not_flags_from_word = click_action.rm_flags;
        
        if(flags_from_word){
            if (flags_from_word.constructor !== Array){
                flags_from_word = [flags_from_word];
            }
            for (var f in flags_from_word){
                var flag = flags_from_word[f];
                if (typeof flag == 'string'){
                    var flag_name = flag;
                    if (!flags[flag_name]){
                        return true;
                    }
                }
                else{
                    return true;
                }
            }
        }
        
        if(not_flags_from_word){
            if (not_flags_from_word.constructor !== Array){
                not_flags_from_word = [not_flags_from_word];
            }
            for (var f in not_flags_from_word){
                var flag = not_flags_from_word[f];
                if (typeof flag == 'string'){
                    var flag_name = flag;
                    if (flags[flag_name] || timed_events[flag_name]){
                        return true;
                    }
                }
                else{
                    return true;
                }
            }
        }
    return false;
    }
    
    var conditions_met = function(condition, flags, text_flags, timed_events){
        var all_flags = Object.assign({}, flags, text_flags, temporary_flags);
        var condition_flags = condition.flags;
        if (condition_flags){
            if (condition_flags.constructor !== Array){
            condition_flags = [condition_flags];
            }
            for (var f in condition_flags){
                var flag = condition_flags[f];
                var value = 0;
                var flag_name;
                if (flag===true){
                    continue;
                }
                if (typeof flag == 'string'){
                    flag_name = flag;
                    value = 1;
                }
                else{
                    for (flag_name in flag){
                        value = flag[flag_name]
                    }
                }
  
                if ((!all_flags[flag_name] || all_flags[flag_name]<value)  && !timed_events[flag_name]){
                    return false;
                }
            }
        }
        var condition_not_flags = condition.not_flags;
        if (condition_not_flags){
            if (condition_not_flags.constructor !== Array){
                condition_not_flags = [condition_not_flags];
            }
            for (var f in condition_not_flags){
                var flag = condition_not_flags[f];
                var value = 0;
                var flag_name;
                if (typeof flag == 'string'){
                    flag_name = flag;
                    value = 1;
                }
                else{
                    for (flag_name in flag){
                        value = flag[flag_name]
                    }
                }
                if ((all_flags[flag_name] && all_flags[flag_name]>=value) ||  timed_events[flag_name]){
                    return false;
                }
            }
        }

        var condition_words = condition.words;
        if (condition_words){
            if (condition_words.constructor !== Array){
            condition_words = [condition_words];
            }
            for (var f in condition_words){
                var word = condition_words[f];
                var value = 0;
                var word_name;
                if (word===true){
                    continue;
                }
                if (typeof word == 'string'){
                    word_name = word;
                    value = 1;
                }
                else{
                    for (word_name in word){
                        value = word[word_name]
                    }
                }
  
                if ((!word_keys[word_name] || word_keys[word_name]<value)){
                    return false;
                }
            }
        }
        
        var condition_prob = condition.prob;
        if (condition_prob){
            if (Math.random()>condition_prob){
                return false;
            }
        }

        
        return true;
    }
    
    var function_array_to_function = function(function_array){
        return function(){
            for (var f in function_array){
                function_array[f]();
            }
        }
    }
    
    var delete_unused_text_flags = function(text_flags, new_substituted_text){
        for (var flag in text_flags){
            if (! new_substituted_text.includes(flag)){
                delete text_flags[flag];
            }
        }
    }

    var push_story_state = function(previous_states, text_flags,flags,substitute_text_dictionary,previous_substituted_text,new_substituted_text, p_styles, t_styles){
        previous_states.unshift([clone_object(text_flags),clone_object(flags),clone_object(substitute_text_dictionary),clone_object(previous_substituted_text),clone_object(new_substituted_text), clone_object(p_styles), clone_object(t_styles), clone_object(timed_events), clone_object(temporary_flags)]);
        if (previous_states.length>20){
            previous_states.length=20;
        }
    }
    
    //Functions to help with editing
    
    var editing = function(){
        html_story_yaml.value=story_yaml;
        add_functions_to_buttons();
        add_functions_to_content_editable();
        set_editor();
    }
    
    function set_editor(){
	if (typeof ace !== 'undefined') {
            editor = ace.edit("editor");
            editor.setTheme("ace/theme/monokai");
            editor.getSession().setMode("ace/mode/yaml");
            editor.setValue(story_yaml);
            editor.getSession().setUseWrapMode(true);
            editor.getSession().setTabSize(2);
            editor.getSession().setUseSoftTabs(true);
            editor.setShowPrintMargin(false);

            add_ctrl_s_editor(document.getElementById("editor"), editor);

	}
    }
    
    var add_functions_to_content_editable = function(){
        html_story_yaml.onkeydown=function(e){
            if(e.keyCode==9){
                document.execCommand('insertHTML', false, '  ');
                e.preventDefault();
            }
        }
        add_ctrl_s(html_story_yaml);
    }
    
    var reload_yaml_from_html = function(){
        story = jsyaml.load(html_story_yaml.value);
        start_text= story.begin.text;
        generate_story();
    }
    
    var reload_yaml_from_editor = function(){
        story = jsyaml.load(editor.getValue());
        start_text= story.begin.text;
        generate_story();
    }
    
    var restart_story = function(){
        substitute_text_dictionary = {"start": "rooftop-start", "options":"OPTIONS"};
        previous_substituted_text = [];
        new_substituted_text = [];
        flags = {}; //Flags set by words. Permanent
        text_flags = {}; // Flags set by text Temporary
        p_styles = []; // Permanent styles applied to the body
        t_styles = []; // Temporary styles applied to the body
        previous_states=[]; // For undo.
        timed_events={};
        generate_story();   
    }
    
    var toggle_editing = function(){
        var editing = document.getElementById("editing")
        editing.classList.toggle('hidden');
    }
    
        
    var add_functions_to_buttons = function(){
        var reload_button = document.getElementById("reload_yaml2");
        reload_button.addEventListener("click", reload_yaml_from_html, false); 
        var reload_button = document.getElementById("reload_yaml1");
        reload_button.addEventListener("click", reload_yaml_from_editor, false); 
        var restart_button = document.getElementById("restart");
        restart_button.addEventListener("click", restart_story, false); 
        var toggle_button = document.getElementById("toggle_editing");
        toggle_button.addEventListener("click", toggle_editing, false); 
        var undo_button = document.getElementById("undo_button");
        undo_button.addEventListener("click", ()=>(undo_story(), generate_story()), false); 
    }
    
    var add_ctrl_s = function(html_story_yaml){
        html_story_yaml.addEventListener("keydown", function(e) {
            if (e.keyCode == 83 && (navigator.platform.match("Mac") ? e.metaKey : e.ctrlKey)) {
                e.preventDefault();
                downloadInnerHtml("story.yaml", html_story_yaml, "yaml");
            }
        }, false);
    }
    
    var add_ctrl_s_editor = function(el, editor){
        el.addEventListener("keydown", function(e) {
            if (e.keyCode == 83 && (navigator.platform.match("Mac") ? e.metaKey : e.ctrlKey)) {
                e.preventDefault();
                downloadText("story.yaml", editor.getValue(), "yaml");
            }
        }, false);
    }
    
    var editing_text_helper = function(present_text_keys){
        var subs = document.getElementById("subs");
        var subs2 = document.getElementById("subs2");
        var flags_html = document.getElementById("flags")
        
        subs.innerHTML = "";
        subs2.innerHTML = "";

        for ( var p in substitute_text_dictionary){
            if (present_text_keys.includes(substitute_text_dictionary[p])){
                subs.innerHTML += "[" + p + " -> "+ substitute_text_dictionary[p] + "] <br>";
                subs2.innerHTML += " "+new_substituted_text[p] + "<br>";

            }
        }
        flags_html.innerHTML = "";

        for ( var f in text_flags){
            flags_html.innerHTML+= "["+f+":"+text_flags[f]+"] ";
        }
        flags_html.innerHTML += "<br>";

        for ( var f in flags){
            flags_html.innerHTML+= "["+f+":"+flags[f]+"] ";
        }
    }
    
    var downloadInnerHtml = function(filename, el, mimeType) {
        var elHtml = el.value;
        downloadText(filename, elHtml, mimeType);
    }
    
    var downloadText = function(filename, text, mimeType) {
        var link = document.createElement('a');
        mimeType = mimeType || 'text/plain';
        link.setAttribute('download', filename);
        link.setAttribute('href', 'data:' + mimeType + ';charset=utf-8,' + encodeURIComponent(text));
        link.click(); 
    }
    
    //Extra helpers
    
    var clone_object = function(obj) {
        if (obj === null || typeof obj !== 'object') {
            return obj;
        }

        var temp = new obj.constructor(); // give temp the original obj's constructor
        for (var key in obj) {
            temp[key] = clone_object(obj[key]);
        }

        return temp;
    }
    
    //Timed events
    
    var clean_timed_events = function(){
        for( var index in timed_events){
            var event = timed_events[index];
            if (event.get_end_event()){
                delete timed_events[index];
            }
        }
    }
    
    var timed_run = function (){
        //do stuff
        var timed_html = document.getElementById("timed_words");
        timed_html.innerHTML="";
        
        
        for( var index in timed_events){
            var event = timed_events[index];
            timed_html.innerHTML+= event.get_word_copy()+" ";
            event.update_time(0.1);
            event.update_style();
            if (text_flags['END']){
                delete timed_events[index];
            }
            else{
                 event.perform_action();
            }
        }
        setTimeout(timed_run, 100);
    }
    
    var timed_event_word = function(word_id, max_time, action, word_text, condition_continue){
        this.word_id = word_id;
        this.max_time = max_time;
        this.action = action;
        this.time = 0;
        this.word_text = word_text;
        this.condition_continue = condition_continue;
        this.update_style = function(){
            var words_html = document.getElementsByClassName(this.word_id);
            var len_words = words_html.length;
            for (var i =0; i<len_words; i++){
                var word_html = words_html[i];
                if (word_html !== null && word_html !== undefined){
                    var percentage = this.time / this.max_time * 100;
                    word_html.style.background = " -webkit-linear-gradient( left, rgba(0,0,0,1) " + percentage +"%, rgba(255,255,255,0) "+ (percentage+10)+ "%)";
                }
            }
        }
        this.remove_style = function(){
            var word_html = document.getElementById(word_id);
            if (word_html !== null){
                word_html.style.background = "";
            }
        }
        this.update_time = function(added_time){
            this.time += added_time;
            this.update_style();
        }
        
        this.perform_action = function(){
            if (this.time > this.max_time){
                // do stuff
                delete timed_events[word_id];
                var my_actions = get_action_function_array(this.action, substitute_text_dictionary, flags, text_flags, this.word_id, timed_events, this.word_text);
                // Remove it from the timed dictionary:
                for (var f in my_actions){
                    my_actions[f]();
                }
                this.remove_style();
                generate_story(false);
                return true;  
            }
            return false;   
        }
        
        this.get_word_copy = function(){
            var word = get_dummy_word();
            word.classList.add(this.word_id);
            word.innerHTML = this.word_text;
            return word.outerHTML;
        }
        
        this.get_end_event = function(){
            if(!this.condition_continue || conditions_met(this.condition_continue, flags, text_flags, timed_events)){
                return false;
            }
            return true;
        }
        
    }
    
    
    //Generate story
    
    var generate_story = function(push_state){
        if(push_state===undefined || push_state == true){
            push_story_state(previous_states, text_flags,flags,substitute_text_dictionary,previous_substituted_text,new_substituted_text, p_styles, t_styles);
        }
        new_substituted_text={};
        var present_text_keys = [];
        word_keys = {}
        var text = substitute_text(start_text, substitute_text_dictionary, story, previous_substituted_text, new_substituted_text, undefined, flags, text_flags, present_text_keys, timed_events);
        previous_substituted_text=new_substituted_text;
        delete_unused_text_flags(text_flags, present_text_keys);
        clean_timed_events();
        decrease_temporary_flags(temporary_flags);
        proccess_words(html_story, text, story, substitute_text_dictionary, flags, text_flags, timed_events);
        var body = document.getElementsByTagName("body")[0]; //document.getElementById("text-background");
        apply_styles_to_el(t_styles,p_styles,body);
        
        editing_text_helper(present_text_keys);        
    }   
    
    timed_run();
    editing();
    
    
    generate_story();

};
