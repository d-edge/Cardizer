using System;
using Dedge;
namespace AppCs
{
    class Program
    {
        static void Main(string[] args)
        {
            var card = Cardizer.GenerateVisa();
            Console.WriteLine(card);
        }
    }
}
