
window.addEventListener('load', (event) => {
    // 対象とするノードを取得
    const target = document.getElementById('source-view-current-line');
    // オブザーバインスタンスを作成
    const observer = new MutationObserver((mutations) => {
        const id = target.dataset.currentLineId;
        const line = document.getElementById(id);
        if (line) {
            // Element.scrollIntoViewIfNeeded() を使いたいところだが、
            // 対応状況が微妙なので。
            line.scrollIntoView({ behavior: "smooth", block: "center" });
        }
    });

    // オブザーバの設定
    const config = { attributes: true };

    // 対象ノードとオブザーバの設定を渡す
    observer.observe(target, config);
});
