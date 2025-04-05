package ahc.dms.controller;

import ahc.dms.dao._payloads.ApiResponse;
import ahc.dms.dao._payloads.CommentDto;
import ahc.dms.dao.services.CommentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/blog")
public class CommentController {

    @Autowired
    private CommentService commentService;

    @PostMapping("/user/{userId}/post/{postId}/comments")
    public ResponseEntity<CommentDto> createComment(
            @RequestBody CommentDto commentDto,
            @PathVariable("userId") Integer userId,
            @PathVariable("postId") Integer postId
    ) {
        CommentDto newCommentDto = commentService.createComment(commentDto, userId, postId);
        return new ResponseEntity<>(newCommentDto, HttpStatus.CREATED);
    }

    @DeleteMapping("/comments/{commentId}")
    public ResponseEntity<ApiResponse> createComment(@PathVariable("commentId") Integer commentId) {
        commentService.deleteComment(commentId);
        return new ResponseEntity<>(new ApiResponse("comment deleted", true), HttpStatus.CREATED);
    }

}
