const player = new Plyr(
    '#audio-player-internal',
    {
        controls: [
            'play-large',
            'play',
            'current-time',
            'progress',
            'duration',
            'mute',
            'volume',
            'download'
        ],
        keyboard: {
            focused: true,
            global: true
        },
        seekTime: 5
    }
);
const cover = document.getElementById("cover-container");
const overlay = document.getElementById("cover-overlay");

cover.onclick = function () {
    player.togglePlay();
};

player.on('play', (_) => {
    overlay.style.display = "none";
});

player.on('pause', (_) => {
    overlay.style.display = "flex";
});

document.addEventListener('keyup', event => {
    if (event.code === 'Space') {
        player.togglePlay();
    }
});
