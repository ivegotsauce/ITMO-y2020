package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.PostAndTags;

@Component
public class PostAndTagsValidator implements Validator {
    @Override
    public boolean supports(Class<?> clazz) {
        return PostAndTags.class.equals(clazz) || Post.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostAndTags postAndTags = (PostAndTags) target;
            for (String tag : postAndTags.getTags().split("\\s+")) {
                if (!(tag.matches("[a-z]+") && tag.length() >= 1 && tag.length() <= 15)) {
                    errors.rejectValue("tags", "tags.invalid-tag", "all tags are expected to contain from 1 to 15 lowercase Latin letters");
                    break;
                }
            }
        }
    }
}
